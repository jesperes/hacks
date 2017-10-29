
# parsetab.py
# This file is automatically generated. Do not edit.

_lr_method = 'SLR'

_lr_signature = '\xbb\xa6\xe2\xf1d+\xb95i~\x92#I1\xfb\x1f'

_lr_action_items = {'LPAR':([33,8,31,23,24,34,17,26,27,32,19,28,20,29,30,22,],[-16,9,-18,-13,-20,-17,-12,20,20,-15,20,20,20,20,-19,-14,]),'LBRACE':([14,],[17,]),'RBRACE':([24,33,31,23,34,17,19,32,30,22,],[-20,-16,-18,-13,-17,-12,21,-15,-19,-14,]),'INT':([4,21,16,9,1,0,15,],[-3,-11,-4,5,5,-2,5,]),'LIST':([4,15,21,16,9,1,0,],[-3,6,-11,-4,6,6,-2,]),'ASTERISK':([32,30,22,25,34,24,33,31,],[29,-19,29,29,-17,-20,29,-18,]),'RPAR':([32,30,12,13,25,10,33,31,34,24,18,],[-15,-19,14,-10,30,-8,-16,-18,-17,-20,-9,]),'PLUS':([31,33,24,34,25,22,30,32,],[-18,-16,-20,-17,27,27,-19,-15,]),'SLASH':([24,34,33,31,25,32,22,30,],[-20,-17,26,-18,26,26,26,-19,]),'INTEGER':([17,26,23,31,33,32,34,24,29,22,30,19,27,20,28,],[-12,24,-13,-18,-16,-15,-17,-20,24,-14,-19,24,24,24,24,]),'STRING':([15,21,4,9,16,1,0,],[3,-11,-3,3,-4,3,-2,]),'IDENTIFIER':([3,5,6,7,11,],[-6,-5,-7,8,13,]),'MINUS':([33,31,34,25,32,24,22,30,],[-16,-18,-17,28,-15,-20,28,-19,]),'COMMA':([12,10,13,18,],[15,-8,-10,-9,]),'$':([0,16,1,2,4,21,],[-2,-4,-1,0,-3,-11,]),}

_lr_action = { }
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
       _lr_action[(_x,_k)] = _y
del _lr_action_items

_lr_goto_items = {'definition':([1,],[4,]),'code_block':([14,],[16,]),'statements':([17,],[19,]),'expr':([27,26,29,28,19,20,],[32,31,34,33,22,25,]),'arg_decl':([15,9,],[18,10,]),'tl':([0,],[2,]),'type_expr':([15,1,9,],[11,7,11,]),'statement':([19,],[23,]),'arg_decl_list':([9,],[12,]),'definitions':([0,],[1,]),}

_lr_goto = { }
for _k, _v in _lr_goto_items.items():
   for _x,_y in zip(_v[0],_v[1]):
       _lr_goto[(_x,_k)] = _y
del _lr_goto_items
_lr_productions = [
  ("S'",1,None,None,None),
  ('tl',1,'p_tl','./parser.py',13),
  ('definitions',0,'p_definitions_empty','./parser.py',17),
  ('definitions',2,'p_definitions','./parser.py',21),
  ('definition',6,'p_definition','./parser.py',25),
  ('type_expr',1,'p_type_expr','./parser.py',29),
  ('type_expr',1,'p_type_expr','./parser.py',30),
  ('type_expr',1,'p_type_expr','./parser.py',31),
  ('arg_decl_list',1,'p_arg_decl_list_empty','./parser.py',35),
  ('arg_decl_list',3,'p_arg_decl_list','./parser.py',39),
  ('arg_decl',2,'p_arg_decl','./parser.py',43),
  ('code_block',3,'p_code_block','./parser.py',47),
  ('statements',0,'p_statements_empty','./parser.py',51),
  ('statements',2,'p_statements','./parser.py',55),
  ('statement',1,'p_statement','./parser.py',59),
  ('expr',3,'p_expr_plus','./parser.py',63),
  ('expr',3,'p_expr_minus','./parser.py',67),
  ('expr',3,'p_expr_times','./parser.py',71),
  ('expr',3,'p_expr_divide','./parser.py',75),
  ('expr',3,'p_expr_paren','./parser.py',79),
  ('expr',1,'p_expr_int','./parser.py',83),
]
