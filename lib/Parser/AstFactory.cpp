//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "ParserPch.h"

/*****************************************************************************
The following set of routines allocate parse tree nodes of various kinds.
They catch an exception on out of memory.
*****************************************************************************/
static const int g_mpnopcbNode[] =
{
#define PTNODE(nop,sn,pc,nk,ok,json) kcbPn##nk,
#include "ptlist.h"
};

const Js::RegSlot NoRegister = (Js::RegSlot)-1;
const Js::RegSlot OneByteRegister = (Js::RegSlot_OneByte)-1;

void AstFactory::InitNode(OpCode nop,ParseNodePtr pnode) {
    pnode->nop = nop;
    pnode->grfpn = PNodeFlags::fpnNone;
    pnode->location = NoRegister;
    pnode->emitLabels = false;
    pnode->isUsed = true;
    pnode->notEscapedUse = false;
    pnode->isInList = false;
    pnode->isCallApplyTargetLoad = false;
}

// Create nodes using Arena
ParseNodePtr
AstFactory::StaticCreateBlockNode(ArenaAllocator* alloc, charcount_t ichMin , charcount_t ichLim, int blockId, PnodeBlockType blockType)
{
    ParseNodePtr pnode = StaticCreateNodeT<knopBlock>(alloc, ichMin, ichLim);
    InitBlockNode(pnode, blockId, blockType);
    return pnode;
}

void AstFactory::InitBlockNode(ParseNodePtr pnode, int blockId, PnodeBlockType blockType)
{
    Assert(pnode->nop == knopBlock);
    pnode->sxBlock.pnodeScopes = nullptr;
    pnode->sxBlock.pnodeNext = nullptr;
    pnode->sxBlock.scope = nullptr;
    pnode->sxBlock.enclosingBlock = nullptr;
    pnode->sxBlock.pnodeLexVars = nullptr;
    pnode->sxBlock.pnodeStmt = nullptr;
    pnode->sxBlock.pnodeLastValStmt = nullptr;

    pnode->sxBlock.callsEval = false;
    pnode->sxBlock.childCallsEval = false;
    pnode->sxBlock.blockType = blockType;
    pnode->sxBlock.blockId = blockId;

    if (blockType != PnodeBlockType::Regular)
    {
        pnode->grfpn |= PNodeFlags::fpnSyntheticNode;
    }
}

// Create Node with limit
template <OpCode nop>
ParseNodePtr AstFactory::CreateNodeT(charcount_t ichMin,charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    ParseNodePtr pnode = StaticCreateNodeT<nop>(&parser->m_nodeAllocator, ichMin, ichLim);

    Assert(parser->m_pCurrentAstSize != NULL);
    *parser->m_pCurrentAstSize += GetNodeSize<nop>();

    return pnode;
}

ParseNodePtr AstFactory::CreateNode(OpCode nop)
{
    return CreateNode(nop, parser->m_pscan ? parser->m_pscan->IchMinTok() : 0);
}

ParseNodePtr AstFactory::CreateDeclNode(OpCode nop, IdentPtr pid, SymbolType symbolType, bool errorOnRedecl)
{
    ParseNodePtr pnode = CreateNode(nop);

    pnode->sxVar.InitDeclNode(pid, NULL);

    if (symbolType != STUnknown)
    {
        pnode->sxVar.sym = parser->AddDeclForPid(pnode, pid, symbolType, errorOnRedecl);
    }

    return pnode;
}

ParseNodePtr AstFactory::CreateBlockNode(PnodeBlockType blockType)
{
    ParseNodePtr pnode = CreateNode(knopBlock);
    InitBlockNode(pnode, parser->m_nextBlockId++, blockType);
    return pnode;
}

#if DBG
void VerifyNodeSize(OpCode nop, int size)
{
    Assert(nop >= 0 && nop < knopLim);
    __analysis_assume(nop < knopLim);
    Assert(g_mpnopcbNode[nop] == size);
}
#endif

ParseNodePtr AstFactory::StaticCreateBinNode(OpCode nop, ParseNodePtr pnode1,
                                   ParseNodePtr pnode2,ArenaAllocator* alloc)
{
    DebugOnly(VerifyNodeSize(nop, kcbPnBin));
    ParseNodePtr pnode = (ParseNodePtr)alloc->Alloc(kcbPnBin);
    InitNode(nop, pnode);

    pnode->sxBin.pnodeNext = nullptr;
    pnode->sxBin.pnode1 = pnode1;
    pnode->sxBin.pnode2 = pnode2;

    // Statically detect if the add is a concat
    if (!PHASE_OFF1(Js::ByteCodeConcatExprOptPhase))
    {
        // We can't flatten the concat expression if the LHS is not a flatten concat already
        // e.g.  a + (<str> + b)
        //      Side effect of ToStr(b) need to happen first before ToStr(a)
        //      If we flatten the concat expression, we will do ToStr(a) before ToStr(b)
        if ((nop == knopAdd) && (pnode1->CanFlattenConcatExpr() || pnode2->nop == knopStr))
        {
            pnode->grfpn |= fpnCanFlattenConcatExpr;
        }
    }

    return pnode;
}

// Create nodes using parser allocator

ParseNodePtr AstFactory::CreateNode(OpCode nop, charcount_t ichMin)
{
    bool nodeAllowed = parser->IsNodeAllowedInCurrentDeferralState(nop);
    Assert(nodeAllowed);

    Assert(nop >= 0 && nop < knopLim);
    ParseNodePtr pnode;
    int cb = (nop >= knopNone && nop < knopLim) ? g_mpnopcbNode[nop] : g_mpnopcbNode[knopEmpty];

    pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(cb);
    Assert(pnode != nullptr);

    if (!parser->m_deferringAST)
    {
        Assert(parser->m_pCurrentAstSize != nullptr);
        *parser->m_pCurrentAstSize += cb;
    }

    InitNode(nop,pnode);

    // default - may be changed
    pnode->ichMin = ichMin;
    if (parser->m_pscan!= nullptr) {
      pnode->ichLim = parser->m_pscan->IchLimTok();
    }
    else pnode->ichLim=0;

    return pnode;
}

ParseNodePtr AstFactory::CreateUniNode(OpCode nop, ParseNodePtr pnode1)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnUni));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnUni);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnUni;

    InitNode(nop, pnode);

    pnode->sxUni.pnode1 = pnode1;
    if (nullptr == pnode1)
    {
        // no ops
        pnode->ichMin = parser->m_pscan->IchMinTok();
        pnode->ichLim = parser->m_pscan->IchLimTok();
    }
    else
    {
        // 1 op
        pnode->ichMin = pnode1->ichMin;
        pnode->ichLim = pnode1->ichLim;
        this->parser->CheckArguments(pnode);
    }
    return pnode;
}

ParseNodePtr AstFactory::CreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2)
{
    Assert(!this->parser->m_deferringAST);
    charcount_t ichMin;
    charcount_t ichLim;

    if (nullptr == pnode1)
    {
        // no ops
        Assert(nullptr == pnode2);
        ichMin = parser->m_pscan->IchMinTok();
        ichLim = parser->m_pscan->IchLimTok();
    }
    else
    {
        if (nullptr == pnode2)
        {
            // 1 op
            ichMin = pnode1->ichMin;
            ichLim = pnode1->ichLim;
        }
        else
        {
            // 2 ops
            ichMin = pnode1->ichMin;
            ichLim = pnode2->ichLim;
            if (nop != knopDot && nop != knopIndex)
            {
                this->parser->CheckArguments(pnode2);
            }
        }
        if (nop != knopDot && nop != knopIndex)
        {
            this->parser->CheckArguments(pnode1);
        }
    }

    return CreateBinNode(nop, pnode1, pnode2, ichMin, ichLim);
}

ParseNodePtr AstFactory::CreateTriNode(OpCode nop, ParseNodePtr pnode1,
                                   ParseNodePtr pnode2, ParseNodePtr pnode3)
{
    charcount_t ichMin;
    charcount_t ichLim;

    if (nullptr == pnode1)
    {
        // no ops
        Assert(nullptr == pnode2);
        Assert(nullptr == pnode3);
        ichMin = parser->m_pscan->IchMinTok();
        ichLim = parser->m_pscan->IchLimTok();
    }
    else if (nullptr == pnode2)
    {
        // 1 op
        Assert(nullptr == pnode3);
        ichMin = pnode1->ichMin;
        ichLim = pnode1->ichLim;
    }
    else if (nullptr == pnode3)
    {
        // 2 op
        ichMin = pnode1->ichMin;
        ichLim = pnode2->ichLim;
    }
    else
    {
        // 3 ops
        ichMin = pnode1->ichMin;
        ichLim = pnode3->ichLim;
    }

    return CreateTriNode(nop, pnode1, pnode2, pnode3, ichMin, ichLim);
}

ParseNodePtr AstFactory::CreateBlockNode(charcount_t ichMin,charcount_t ichLim, PnodeBlockType blockType)
{
    return StaticCreateBlockNode(&parser->m_nodeAllocator, ichMin, ichLim, this->parser->m_nextBlockId++, blockType);
}

ParseNodePtr
AstFactory::CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2,charcount_t ichMin,charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnCall));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnCall);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnCall;

    InitNode(nop, pnode);

    pnode->sxCall.pnodeTarget = pnode1;
    pnode->sxCall.pnodeArgs = pnode2;
    pnode->sxCall.argCount = 0;
    pnode->sxCall.spreadArgCount = 0;
    pnode->sxCall.callOfConstants = false;
    pnode->sxCall.isApplyCall = false;
    pnode->sxCall.isEvalCall = false;

    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;

    return pnode;
}

// Create Node with scanner limit
template <OpCode nop>
ParseNodePtr AstFactory::CreateNodeWithScanner()
{
    Assert(parser->m_pscan != nullptr);
    return CreateNodeWithScanner<nop>(parser->m_pscan->IchMinTok());
}

#define PTNODE(nop,sn,pc,nk,ok,json) \
    template ParseNodePtr AstFactory::CreateNodeWithScanner<nop>();
#include "ptlist.h"

template <OpCode nop>
ParseNodePtr AstFactory::CreateNodeWithScanner(charcount_t ichMin)
{
    Assert(parser->m_pscan != nullptr);
    return CreateNodeT<nop>(ichMin, parser->m_pscan->IchLimTok());
}

ParseNodePtr AstFactory::CreateProgNodeWithScanner(bool isModuleSource)
{
    ParseNodePtr pnodeProg;

    if (isModuleSource)
    {
        pnodeProg = CreateNodeWithScanner<knopModule>();

        // knopModule is not actually handled anywhere since we would need to handle it everywhere we could
        // have knopProg and it would be treated exactly the same except for import/export statements.
        // We are only using it as a way to get the correct size for PnModule.
        // Consider: Should we add a flag to PnProg which is false but set to true in PnModule?
        //           If we do, it can't be a virtual method since the parse nodes are all in a union.
        pnodeProg->nop = knopProg;
    }
    else
    {
        pnodeProg = CreateNodeWithScanner<knopProg>();
    }

    return pnodeProg;
}

ParseNodePtr AstFactory::CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2)
{
    charcount_t ichMin;
    charcount_t ichLim;

    if (nullptr == pnode1)
    {
        Assert(nullptr == pnode2);
        ichMin = parser->m_pscan->IchMinTok();
        ichLim = parser->m_pscan->IchLimTok();
    }
    else
    {
        if (nullptr == pnode2)
        {
            ichMin = pnode1->ichMin;
            ichLim = pnode1->ichLim;
        }
        else
        {
            ichMin = pnode1->ichMin;
            ichLim = pnode2->ichLim;
        }
        if (pnode1->nop == knopDot || pnode1->nop == knopIndex)
        {
            this->parser->CheckArguments(pnode1->sxBin.pnode1);
        }
    }
    return CreateCallNode(nop, pnode1, pnode2, ichMin, ichLim);
}

ParseNodePtr AstFactory::CreateStrNodeWithScanner(IdentPtr pid)
{
    Assert(!this->parser->m_deferringAST);

    ParseNodePtr pnode = CreateNodeWithScanner<knopStr>();
    pnode->sxPid.pid=pid;
    pnode->grfpn |= PNodeFlags::fpnCanFlattenConcatExpr;
    return pnode;
}

ParseNodePtr AstFactory::CreateIntNodeWithScanner(int32 lw)
{
    Assert(!this->parser->m_deferringAST);
    ParseNodePtr pnode = CreateNodeWithScanner<knopInt>();
    pnode->sxInt.lw = lw;
    return pnode;
}

ParseNodePtr AstFactory::CreateTempNode(ParseNode* initExpr)
{
    ParseNodePtr pnode = CreateNode(knopTemp, (charcount_t)0);
    pnode->sxVar.pnodeInit =initExpr;
    pnode->sxVar.pnodeNext = nullptr;
    return pnode;
}

ParseNodePtr AstFactory::CreateTempRef(ParseNode* tempNode)
{
    ParseNodePtr pnode = CreateUniNode(knopTempRef, tempNode);
    return pnode;
}

ParseNodePtr AstFactory::CreateModuleImportDeclNode(IdentPtr localName)
{
    ParseNodePtr declNode = CreateBlockScopedDeclNode(localName, knopConstDecl);
    Symbol* sym = declNode->sxVar.sym;

    sym->SetIsModuleExportStorage(true);
    sym->SetIsModuleImport(true);

    return declNode;
}

ParseNodePtr AstFactory::CreateVarDeclNode(IdentPtr pid, SymbolType symbolType, bool autoArgumentsObject, ParseNodePtr pnodeFnc, bool errorOnRedecl)
{
    ParseNodePtr pnode = CreateDeclNode(knopVarDecl, pid, symbolType, errorOnRedecl);

    // Append the variable to the end of the current variable list.
    pnode->sxVar.pnodeNext = *parser->m_ppnodeVar;
    *parser->m_ppnodeVar = pnode;
    if (nullptr != pid)
    {
        // this is not a temp - make sure temps go after this node
        parser->m_ppnodeVar = &pnode->sxVar.pnodeNext;
        parser->CheckStrictModeEvalArgumentsUsage(pid, pnode, autoArgumentsObject);
    }

    return pnode;
}

ParseNodePtr AstFactory::CreateBlockScopedDeclNode(IdentPtr pid, OpCode nodeType)
{
    Assert(nodeType == knopConstDecl || nodeType == knopLetDecl);

    ParseNodePtr pnode = CreateDeclNode(nodeType, pid, STVariable, true);

    if (nullptr != pid)
    {
        pid->SetIsLetOrConst();
        parser->AddVarDeclToBlock(pnode);
        // TODO[ianhall]: There appears to be no corresponding check for !buildAST on the CreateModuleImportDeclNode path.  Is this a bug?
        parser->CheckStrictModeEvalArgumentsUsage(pid, pnode);
    }

    return pnode;
}

ParseNodePtr AstFactory::CreateDummyFuncNode(bool fDeclaration)
{
    // Create a dummy node and make it look like the current function declaration.
    // Do this in situations where we want to parse statements without impacting
    // the state of the "real" AST.

    ParseNodePtr pnodeFnc = CreateNode(knopFncDecl);
    pnodeFnc->sxFnc.ClearFlags();
    pnodeFnc->sxFnc.SetDeclaration(fDeclaration);
    pnodeFnc->sxFnc.astSize             = 0;
    pnodeFnc->sxFnc.pnodeName           = nullptr;
    pnodeFnc->sxFnc.pnodeScopes         = nullptr;
    pnodeFnc->sxFnc.pnodeRest           = nullptr;
    pnodeFnc->sxFnc.pid                 = nullptr;
    pnodeFnc->sxFnc.hint                = nullptr;
    pnodeFnc->sxFnc.hintOffset          = 0;
    pnodeFnc->sxFnc.hintLength          = 0;
    pnodeFnc->sxFnc.isNameIdentifierRef = true;
    pnodeFnc->sxFnc.nestedFuncEscapes   = false;
    pnodeFnc->sxFnc.pnodeNext           = nullptr;
    pnodeFnc->sxFnc.pnodeParams         = nullptr;
    pnodeFnc->sxFnc.pnodeVars           = nullptr;
    pnodeFnc->sxFnc.funcInfo            = nullptr;
    pnodeFnc->sxFnc.deferredStub        = nullptr;
    pnodeFnc->sxFnc.nestedCount         = 0;
    pnodeFnc->sxFnc.SetNested(parser->m_currentNodeFunc != nullptr); // If there is a current function, then we're a nested function.
    pnodeFnc->sxFnc.SetStrictMode(parser->IsStrictMode()); // Inherit current strict mode -- may be overridden by the function itself if it contains a strict mode directive.
    pnodeFnc->sxFnc.firstDefaultArg = 0;

    parser->m_pCurrentAstSize = &pnodeFnc->sxFnc.astSize;
    parser->m_currentNodeFunc = pnodeFnc;
    parser->m_pnestedCount = &pnodeFnc->sxFnc.nestedCount;

    return pnodeFnc;
}

// Create node versions with explicit token limits
ParseNodePtr AstFactory::CreateNode(OpCode nop, charcount_t ichMin, charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    Assert(nop >= 0 && nop < knopLim);
    ParseNodePtr pnode;
    __analysis_assume(nop < knopLim);
    int cb = nop >= 0 && nop < knopLim ? g_mpnopcbNode[nop] : kcbPnNone;

    pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(cb);
    Assert(pnode);

    Assert(parser->m_pCurrentAstSize != NULL);
    *parser->m_pCurrentAstSize += cb;

    InitNode(nop,pnode);

    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;

    return pnode;
}

ParseNodePtr AstFactory::CreateNameNode(IdentPtr pid,charcount_t ichMin,charcount_t ichLim) {
  ParseNodePtr pnode = CreateNodeT<knopName>(ichMin,ichLim);
  pnode->sxPid.pid = pid;
  pnode->sxPid.sym=NULL;
  pnode->sxPid.symRef=NULL;
  return pnode;
}

ParseNodePtr AstFactory::CreateUniNode(OpCode nop, ParseNodePtr pnode1, charcount_t ichMin,charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnUni));

    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnUni);

    Assert(parser->m_pCurrentAstSize != NULL);
    *parser->m_pCurrentAstSize += kcbPnUni;

    InitNode(nop, pnode);

    pnode->sxUni.pnode1 = pnode1;

    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;

    return pnode;
}

ParseNodePtr AstFactory::CreateBinNode(OpCode nop, ParseNodePtr pnode1,
                                   ParseNodePtr pnode2,charcount_t ichMin,charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    ParseNodePtr pnode = StaticCreateBinNode(nop, pnode1, pnode2, &parser->m_nodeAllocator);

    Assert(parser->m_pCurrentAstSize != NULL);
    *parser->m_pCurrentAstSize += kcbPnBin;

    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;

    return pnode;
}

ParseNodePtr AstFactory::CreateTriNode(OpCode nop, ParseNodePtr pnode1,
                                   ParseNodePtr pnode2, ParseNodePtr pnode3,
                                   charcount_t ichMin,charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnTri));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnTri);

    Assert(parser->m_pCurrentAstSize != NULL);
    *parser->m_pCurrentAstSize += kcbPnTri;

    InitNode(nop, pnode);

    pnode->sxTri.pnodeNext = NULL;
    pnode->sxTri.pnode1 = pnode1;
    pnode->sxTri.pnode2 = pnode2;
    pnode->sxTri.pnode3 = pnode3;

    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;

    return pnode;
}

ParseNodePtr AstFactory::CreateParamPatternNode(ParseNodePtr pnode1)
{
    ParseNodePtr paramPatternNode = CreateNode(knopParamPattern, pnode1->ichMin, pnode1->ichLim);
    paramPatternNode->sxParamPattern.pnode1 = pnode1;
    paramPatternNode->sxParamPattern.pnodeNext = nullptr;
    paramPatternNode->sxParamPattern.location = Js::Constants::NoRegister;
    return paramPatternNode;
}
