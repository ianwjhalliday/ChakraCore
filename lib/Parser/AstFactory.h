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

    template <OpCode nop>
    ParseNodePtr CreateNode();
    template <OpCode nop>
    ParseNodePtr CreateNode(charcount_t ichMin);
    template <OpCode nop>
    ParseNodePtr CreateNode(charcount_t ichMin, charcount_t ichLim);

    ParseNodePtr CreateStrNode(IdentPtr pid);
    ParseNodePtr CreateIntNode(int32 lw);

    ParseNodePtr CreateUniNode(OpCode nop, ParseNodePtr pnode1);
    ParseNodePtr CreateUniNode(OpCode nop, ParseNodePtr pnode1, charcount_t ichMin, charcount_t ichLim);

    ParseNodePtr CreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2);
    ParseNodePtr CreateBinNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, charcount_t ichMin, charcount_t ichLim);

    ParseNodePtr CreateTriNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, ParseNodePtr pnode3);
    ParseNodePtr CreateTriNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, ParseNodePtr pnode3, charcount_t ichMin, charcount_t ichLim);

    // CreateNameNode(IdentPtr) used to be defined inline; check if perf regression
    ParseNodePtr CreateNameNode(IdentPtr pid);
    ParseNodePtr CreateNameNode(IdentPtr pid, charcount_t ichMin, charcount_t ichLim);

    ParseNodePtr CreateBlockNode(PnodeBlockType blockType = PnodeBlockType::Regular);
    ParseNodePtr CreateBlockNode(charcount_t ichMin, charcount_t ichLim, PnodeBlockType blockType = PnodeBlockType::Regular);

    ParseNodePtr CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2);
    ParseNodePtr CreateCallNode(OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, charcount_t ichMin, charcount_t ichLim);

    template <OpCode nop>
    ParseNodePtr CreateDeclNode(IdentPtr pid, SymbolType symbolType, bool errorOnRedecl = true);

    // Add a var declaration. Only use while parsing. Assumes m_ppnodeVar is pointing to the right place already
    ParseNodePtr CreateVarDeclNode(IdentPtr pid, SymbolType symbolType, bool autoArgumentsObject = false, ParseNodePtr pnodeFnc = nullptr, bool checkReDecl = true);
    // Add a var declaration, during parse tree rewriting. Will setup m_ppnodeVar for the given pnodeFnc
    // This shouldn't be here? Or should the other two not be named Create...()?
    //ParseNodePtr AddVarDeclNode(IdentPtr pid, ParseNodePtr pnodeFnc);
    // Add a 'const' or 'let' declaration.
    ParseNodePtr CreateLetDeclNode(IdentPtr pid);
    ParseNodePtr CreateConstDeclNode(IdentPtr pid);

    ParseNodePtr CreateModuleImportDeclNode(IdentPtr localName);

    ParseNodePtr CreateParamPatternNode(ParseNodePtr pnode1);

    ParseNodePtr CreateProgNode(bool isModuleSource);

    ParseNodePtr CreateDummyFuncNode(bool fDeclaration);

    ParseNodePtr CreateTempNode(ParseNode* initExpr);
    ParseNodePtr CreateTempRef(ParseNode* tempNode);

public:
    // static create nodes using arena allocator; used by AST transformation
    template <OpCode nop>
    static ParseNodePtr StaticCreateNodeT(ArenaAllocator* alloc, charcount_t ichMin = 0, charcount_t ichLim = 0)
    {
        ParseNodePtr pnode = (ParseNodePtr)alloc->Alloc(GetNodeSize<nop>());
        Assert(pnode != nullptr);

        // default min/lim - may be changed
        InitNode(nop, pnode, ichMin, ichLim);

        return pnode;
    }

    static ParseNodePtr StaticCreateBinNode(ArenaAllocator* alloc, OpCode nop, ParseNodePtr pnode1, ParseNodePtr pnode2, charcount_t ichMin = 0, charcount_t ichLim = 0);
    static ParseNodePtr StaticCreateBlockNode(ArenaAllocator* alloc, charcount_t ichMin = 0, charcount_t ichLim = 0, int blockId = -1, PnodeBlockType blockType = PnodeBlockType::Regular);

private:
    template <OpCode nop>
    ParseNodePtr CreateBlockScopedDeclNode(IdentPtr pid);

    void ChooseOpMinLim(ParseNodePtr pnode1, ParseNodePtr pnode2, ParseNodePtr pnode3, charcount_t* pichMin, charcount_t* pichLim);

    static void InitNode(OpCode nop, ParseNodePtr pnode, charcount_t ichMin, charcount_t ichLim);
    static void InitBinNode(ParseNodePtr pnode, ParseNodePtr pnode1, ParseNodePtr pnode2);
    static void InitBlockNode(ParseNodePtr pnode, int blockId, PnodeBlockType blockType);
    static void InitDeclNode(ParseNodePtr pnode, IdentPtr name);

    ParseNodePtr InternalCreateNode(OpCode nop, int cb, charcount_t ichMin, charcount_t ichLim);
    template <OpCode nop> static int GetNodeSize();
};

#define PTNODE(nop,sn,pc,nk,ok,json) \
    template<> inline int AstFactory::GetNodeSize<nop>() { return kcbPn##nk; }
#include "ptlist.h"