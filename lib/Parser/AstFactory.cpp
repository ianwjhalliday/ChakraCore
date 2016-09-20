//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "ParserPch.h"

#if DBG
static const int g_mpnopcbNode[] =
{
#define PTNODE(nop,sn,pc,nk,ok,json) kcbPn##nk,
#include "ptlist.h"
};

void VerifyNodeSize(OpCode nop, int size)
{
    Assert(nop >= 0 && nop < knopLim);
    __analysis_assume(nop < knopLim);
    Assert(g_mpnopcbNode[nop] == size);
}
#endif

template <OpCode nop>
ParseNodePtr AstFactory::CreateNode()
{
    return CreateNode<nop>(parser->m_pscan->IchMinTok());
}

template <OpCode nop>
ParseNodePtr AstFactory::CreateNode(charcount_t ichMin)
{
    return CreateNode<nop>(ichMin, parser->m_pscan->IchLimTok());
}

template <OpCode nop>
ParseNodePtr AstFactory::CreateNode(charcount_t ichMin, charcount_t ichLim)
{
    Assert(parser->IsNodeAllowedInCurrentDeferralState(nop));

    if (!parser->m_deferringAST)
    {
        Assert(parser->m_pCurrentAstSize != nullptr);
        *parser->m_pCurrentAstSize += GetNodeSize<nop>();
    }

    return StaticCreateNodeT<nop>(&parser->m_nodeAllocator, ichMin, ichLim);
}

#define PTNODE(nop,sn,pc,nk,ok,json) \
    template ParseNodePtr AstFactory::CreateNode<nop>();
#include "ptlist.h"


ParseNodePtr AstFactory::CreateStrNode(IdentPtr pid)
{
    Assert(!this->parser->m_deferringAST);

    ParseNodePtr pnode = CreateNode<knopStr>();
    pnode->sxPid.pid = pid;
    pnode->grfpn |= PNodeFlags::fpnCanFlattenConcatExpr;
    return pnode;
}

ParseNodePtr AstFactory::CreateIntNode(int32 lw)
{
    Assert(!this->parser->m_deferringAST);
    ParseNodePtr pnode = CreateNode<knopInt>();
    pnode->sxInt.lw = lw;
    return pnode;
}

ParseNodePtr AstFactory::CreateUniNode(OpCode nop, ParseNodePtr pnode1)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnUni));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnUni);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnUni;

    charcount_t ichMin;
    charcount_t ichLim;
    if (nullptr == pnode1)
    {
        // no ops
        ichMin = parser->m_pscan->IchMinTok();
        ichLim = parser->m_pscan->IchLimTok();
    }
    else
    {
        // 1 op
        ichMin = pnode1->ichMin;
        ichLim = pnode1->ichLim;
    }

    InitNode(nop, pnode, ichMin, ichLim);

    pnode->sxUni.pnode1 = pnode1;

    if (pnode1 != nullptr)
    {
        // Checking for the `arguments` identifier shouldn't be the responsibility of AstFactory
        this->parser->CheckArguments(pnode);
    }

    return pnode;
}

ParseNodePtr AstFactory::CreateUniNode(OpCode nop, ParseNodePtr pnode1, charcount_t ichMin, charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnUni));

    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnUni);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnUni;

    InitNode(nop, pnode, ichMin, ichLim);

    pnode->sxUni.pnode1 = pnode1;

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

ParseNodePtr AstFactory::CreateBinNode(OpCode nop, ParseNodePtr pnode1,
                                       ParseNodePtr pnode2, charcount_t ichMin, charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    ParseNodePtr pnode = StaticCreateBinNode(&parser->m_nodeAllocator, nop, pnode1, pnode2, ichMin, ichLim);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnBin;

    return pnode;
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

ParseNodePtr AstFactory::CreateTriNode(OpCode nop, ParseNodePtr pnode1,
                                       ParseNodePtr pnode2, ParseNodePtr pnode3,
                                       charcount_t ichMin, charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnTri));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnTri);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnTri;

    InitNode(nop, pnode, ichMin, ichLim);

    pnode->sxTri.pnodeNext = nullptr;
    pnode->sxTri.pnode1 = pnode1;
    pnode->sxTri.pnode2 = pnode2;
    pnode->sxTri.pnode3 = pnode3;

    return pnode;
}

ParseNodePtr AstFactory::CreateNameNode(IdentPtr pid)
{
    ParseNodePtr pnode = CreateNode<knopName>();
    pnode->sxPid.pid = pid;
    pnode->sxPid.sym = nullptr;
    pnode->sxPid.symRef = nullptr;
    return pnode;
}

ParseNodePtr AstFactory::CreateNameNode(IdentPtr pid, charcount_t ichMin, charcount_t ichLim)
{
    ParseNodePtr pnode = CreateNode<knopName>(ichMin, ichLim);
    pnode->sxPid.pid = pid;
    pnode->sxPid.sym = nullptr;
    pnode->sxPid.symRef = nullptr;
    return pnode;
}

ParseNodePtr AstFactory::CreateBlockNode(PnodeBlockType blockType)
{
    ParseNodePtr pnode = CreateNode<knopBlock>();
    InitBlockNode(pnode, parser->m_nextBlockId++, blockType);
    return pnode;
}

ParseNodePtr AstFactory::CreateBlockNode(charcount_t ichMin, charcount_t ichLim, PnodeBlockType blockType)
{
    return StaticCreateBlockNode(&parser->m_nodeAllocator, ichMin, ichLim, this->parser->m_nextBlockId++, blockType);
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

ParseNodePtr
AstFactory::CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, charcount_t ichMin, charcount_t ichLim)
{
    Assert(!this->parser->m_deferringAST);
    DebugOnly(VerifyNodeSize(nop, kcbPnCall));
    ParseNodePtr pnode = (ParseNodePtr)parser->m_nodeAllocator.Alloc(kcbPnCall);

    Assert(parser->m_pCurrentAstSize != nullptr);
    *parser->m_pCurrentAstSize += kcbPnCall;

    InitNode(nop, pnode, ichMin, ichLim);

    pnode->sxCall.pnodeTarget = pnode1;
    pnode->sxCall.pnodeArgs = pnode2;
    pnode->sxCall.argCount = 0;
    pnode->sxCall.spreadArgCount = 0;
    pnode->sxCall.callOfConstants = false;
    pnode->sxCall.isApplyCall = false;
    pnode->sxCall.isEvalCall = false;

    return pnode;
}

template <OpCode nop>
ParseNodePtr AstFactory::CreateDeclNode(IdentPtr pid, SymbolType symbolType, bool errorOnRedecl)
{
    // CreateDeclNode is actually used by the bytecode generator when
    // parser->m_pscan is null, so pass min/lim explicitly with default
    // zero values. This and CreateTempNode are the only cases where we
    // call CreateNode without a scanner present.
    charcount_t ichMin = parser->m_pscan != nullptr ? parser->m_pscan->IchMinTok() : 0;
    charcount_t ichLim = parser->m_pscan != nullptr ? parser->m_pscan->IchLimTok() : 0;
    ParseNodePtr pnode = CreateNode<nop>(ichMin, ichLim);

    InitDeclNode(pnode, pid);

    if (symbolType != STUnknown)
    {
        pnode->sxVar.sym = parser->AddDeclForPid(pnode, pid, symbolType, errorOnRedecl);
    }

    return pnode;
}

ParseNodePtr AstFactory::CreateVarDeclNode(IdentPtr pid, SymbolType symbolType, bool autoArgumentsObject, ParseNodePtr pnodeFnc, bool errorOnRedecl)
{
    ParseNodePtr pnode = CreateDeclNode<knopVarDecl>(pid, symbolType, errorOnRedecl);

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

ParseNodePtr AstFactory::CreateLetDeclNode(IdentPtr pid)
{
    return CreateBlockScopedDeclNode<knopLetDecl>(pid);
}

ParseNodePtr AstFactory::CreateConstDeclNode(IdentPtr pid)
{
    return CreateBlockScopedDeclNode<knopConstDecl>(pid);
}

ParseNodePtr AstFactory::CreateModuleImportDeclNode(IdentPtr localName)
{
    ParseNodePtr declNode = CreateConstDeclNode(localName);
    Symbol* sym = declNode->sxVar.sym;

    sym->SetIsModuleExportStorage(true);
    sym->SetIsModuleImport(true);

    return declNode;
}

ParseNodePtr AstFactory::CreateParamPatternNode(ParseNodePtr pnode1)
{
    ParseNodePtr paramPatternNode = CreateNode<knopParamPattern>(pnode1->ichMin, pnode1->ichLim);
    paramPatternNode->sxParamPattern.pnode1 = pnode1;
    paramPatternNode->sxParamPattern.pnodeNext = nullptr;
    paramPatternNode->sxParamPattern.location = Js::Constants::NoRegister;
    return paramPatternNode;
}

ParseNodePtr AstFactory::CreateProgNode(bool isModuleSource)
{
    ParseNodePtr pnodeProg;

    if (isModuleSource)
    {
        pnodeProg = CreateNode<knopModule>();

        // knopModule is not actually handled anywhere since we would need to handle it everywhere we could
        // have knopProg and it would be treated exactly the same except for import/export statements.
        // We are only using it as a way to get the correct size for PnModule.
        // Consider: Should we add a flag to PnProg which is false but set to true in PnModule?
        //           If we do, it can't be a virtual method since the parse nodes are all in a union.
        pnodeProg->nop = knopProg;
    }
    else
    {
        pnodeProg = CreateNode<knopProg>();
    }

    return pnodeProg;
}

ParseNodePtr AstFactory::CreateDummyFuncNode(bool fDeclaration)
{
    // Create a dummy node and make it look like the current function declaration.
    // Do this in situations where we want to parse statements without impacting
    // the state of the "real" AST.

    ParseNodePtr pnodeFnc = CreateNode<knopFncDecl>();
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

ParseNodePtr AstFactory::CreateTempNode(ParseNode* initExpr)
{
    // CreateTempNode is actually used by the bytecode generator when
    // parser->m_pscan is null, so pass min/lim explicitly with default
    // zero values. This and CreateDeclNode are the only cases where we
    // call CreateNode without a scanner present.
    ParseNodePtr pnode = CreateNode<knopTemp>(0, 0);
    pnode->sxVar.pnodeInit = initExpr;
    pnode->sxVar.pnodeNext = nullptr;
    return pnode;
}

ParseNodePtr AstFactory::CreateTempRef(ParseNode* tempNode)
{
    ParseNodePtr pnode = CreateUniNode(knopTempRef, tempNode);
    return pnode;
}

ParseNodePtr AstFactory::StaticCreateBinNode(ArenaAllocator* alloc, OpCode nop,
                                             ParseNodePtr pnode1, ParseNodePtr pnode2,
                                             charcount_t ichMin, charcount_t ichLim)
{
    DebugOnly(VerifyNodeSize(nop, kcbPnBin));
    ParseNodePtr pnode = (ParseNodePtr)alloc->Alloc(kcbPnBin);
    InitNode(nop, pnode, ichMin, ichLim);

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

ParseNodePtr
AstFactory::StaticCreateBlockNode(ArenaAllocator* alloc, charcount_t ichMin, charcount_t ichLim, int blockId, PnodeBlockType blockType)
{
    ParseNodePtr pnode = StaticCreateNodeT<knopBlock>(alloc, ichMin, ichLim);
    InitBlockNode(pnode, blockId, blockType);
    return pnode;
}

template <OpCode nop>
ParseNodePtr AstFactory::CreateBlockScopedDeclNode(IdentPtr pid)
{
    CompileAssert(nop == knopConstDecl || nop == knopLetDecl);

    ParseNodePtr pnode = CreateDeclNode<nop>(pid, STVariable, true);

    if (nullptr != pid)
    {
        pid->SetIsLetOrConst();
        parser->AddVarDeclToBlock(pnode);
        // TODO[ianhall]: There appears to be no corresponding check for !buildAST on the CreateModuleImportDeclNode path.  Is this a bug?
        parser->CheckStrictModeEvalArgumentsUsage(pid, pnode);
    }

    return pnode;
}

void AstFactory::InitNode(OpCode nop, ParseNodePtr pnode, charcount_t ichMin, charcount_t ichLim)
{
    pnode->nop = nop;
    pnode->grfpn = PNodeFlags::fpnNone;
    pnode->ichMin = ichMin;
    pnode->ichLim = ichLim;
    pnode->location = Js::Constants::NoRegister;
    pnode->isUsed = true;
    pnode->emitLabels = false;
    pnode->notEscapedUse = false;
    pnode->isInList = false;
    pnode->isCallApplyTargetLoad = false;
#ifdef EDIT_AND_CONTINUE
    pnode->parent = nullptr;
#endif
}

void AstFactory::InitBlockNode(ParseNodePtr pnode, int blockId, PnodeBlockType blockType)
{
    Assert(pnode->nop == knopBlock);
    pnode->sxBlock.pnodeStmt = nullptr;
    pnode->sxBlock.pnodeLastValStmt = nullptr;
    pnode->sxBlock.pnodeLexVars = nullptr;
    pnode->sxBlock.pnodeScopes = nullptr;
    pnode->sxBlock.pnodeNext = nullptr;
    pnode->sxBlock.scope = nullptr;

    pnode->sxBlock.enclosingBlock = nullptr;
    pnode->sxBlock.blockId = blockId;
    pnode->sxBlock.blockType = blockType;
    pnode->sxBlock.callsEval = false;
    pnode->sxBlock.childCallsEval = false;

    if (blockType != PnodeBlockType::Regular)
    {
        pnode->grfpn |= PNodeFlags::fpnSyntheticNode;
    }
}

void AstFactory::InitDeclNode(ParseNodePtr pnode, IdentPtr name)
{
    Assert(pnode->IsVarLetOrConst());
    pnode->sxVar.pnodeNext = nullptr;
    pnode->sxVar.pid = name;
    pnode->sxVar.sym = nullptr;
    pnode->sxVar.symRef = nullptr;
    pnode->sxVar.pnodeInit = nullptr;
    pnode->sxVar.isSwitchStmtDecl = false;
    pnode->sxVar.isBlockScopeFncDeclVar = false;
}
