//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#pragma once

enum SymbolType : byte;

class AstFactory
{
private:
    Parser* parser;

public:
    AstFactory(Parser* p) : parser(p) { }

    // create nodes using arena allocator; used by AST transformation
    template <OpCode nop>
    static ParseNodePtr StaticCreateNodeT(ArenaAllocator* alloc, charcount_t ichMin = 0, charcount_t ichLim = 0)
    {
        ParseNodePtr pnode = StaticAllocNode<nop>(alloc);
        InitNode(nop, pnode);
        // default - may be changed
        pnode->ichMin = ichMin;
        pnode->ichLim = ichLim;

        return pnode;
    }

    static ParseNodePtr StaticCreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, ArenaAllocator* alloc);
    static ParseNodePtr StaticCreateBlockNode(ArenaAllocator* alloc, charcount_t ichMin = 0, charcount_t ichLim = 0, int blockId = -1, PnodeBlockType blockType = PnodeBlockType::Regular);
    ParseNodePtr CreateNode(OpCode nop, charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateDummyFuncNode(bool fDeclaration);


    ParseNodePtr CreateTriNode(OpCode nop, ParseNodePtr pnode1,
                               ParseNodePtr pnode2, ParseNodePtr pnode3,
                               charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateTempNode(ParseNode* initExpr);
    ParseNodePtr CreateTempRef(ParseNode* tempNode);

    ParseNodePtr CreateNode(OpCode nop);
    ParseNodePtr CreateDeclNode(OpCode nop, IdentPtr pid, SymbolType symbolType, bool errorOnRedecl = true);

    ParseNodePtr CreateNameNode(IdentPtr pid)
    {
        ParseNodePtr pnode = CreateNode(knopName);
        pnode->sxPid.pid = pid;
        pnode->sxPid.sym = NULL;
        pnode->sxPid.symRef = NULL;
        return pnode;
    }
    ParseNodePtr CreateBlockNode(PnodeBlockType blockType = PnodeBlockType::Regular);
    // Creating parse nodes.

    ParseNodePtr CreateNode(OpCode nop, charcount_t ichMin);
    ParseNodePtr CreateTriNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, ParseNodePtr pnode3);

    ParseNodePtr CreateUniNode(OpCode nop, ParseNodePtr pnodeOp);
    ParseNodePtr CreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2);
    ParseNodePtr CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2);

    // Create parse node with token limis
    template <OpCode nop>
    ParseNodePtr CreateNodeT(charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateUniNode(OpCode nop, ParseNodePtr pnode1, charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateBlockNode(charcount_t ichMin, charcount_t ichLim, PnodeBlockType blockType = PnodeBlockType::Regular);
    ParseNodePtr CreateNameNode(IdentPtr pid, charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2,
                               charcount_t ichMin, charcount_t ichLim);
    ParseNodePtr CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2,
                                charcount_t ichMin, charcount_t ichLim);

    // Add a var declaration. Only use while parsing. Assumes m_ppnodeVar is pointing to the right place already
    ParseNodePtr CreateVarDeclNode(IdentPtr pid, SymbolType symbolType, bool autoArgumentsObject = false, ParseNodePtr pnodeFnc = NULL, bool checkReDecl = true);
    // Add a var declaration, during parse tree rewriting. Will setup m_ppnodeVar for the given pnodeFnc
    // This shouldn't be here? Or should the other two not be named Create...()?
    //ParseNodePtr AddVarDeclNode(IdentPtr pid, ParseNodePtr pnodeFnc);
    // Add a 'const' or 'let' declaration.
    ParseNodePtr CreateBlockScopedDeclNode(IdentPtr pid, OpCode nodeType);

    ParseNodePtr CreateModuleImportDeclNode(IdentPtr localName);

    ParseNodePtr CreateParamPatternNode(ParseNodePtr pnode1);

    template <OpCode nop> ParseNodePtr CreateNodeWithScanner();
    template <OpCode nop> ParseNodePtr CreateNodeWithScanner(charcount_t ichMin);
    ParseNodePtr CreateStrNodeWithScanner(IdentPtr pid);
    ParseNodePtr CreateIntNodeWithScanner(int32 lw);
    ParseNodePtr CreateProgNodeWithScanner(bool isModuleSource);

private:
    static void InitNode(OpCode nop, ParseNodePtr pnode);
    static void InitBlockNode(ParseNodePtr pnode, int blockId, PnodeBlockType blockType);

    template <OpCode nop> static int GetNodeSize();

    template <OpCode nop> static ParseNodePtr StaticAllocNode(ArenaAllocator * alloc)
    {
        ParseNodePtr pnode = (ParseNodePtr)alloc->Alloc(GetNodeSize<nop>());
        Assert(pnode != nullptr);
        return pnode;
    }
};

#define PTNODE(nop,sn,pc,nk,ok,json) \
    template<> inline int AstFactory::GetNodeSize<nop>() { return kcbPn##nk; }
#include "ptlist.h"