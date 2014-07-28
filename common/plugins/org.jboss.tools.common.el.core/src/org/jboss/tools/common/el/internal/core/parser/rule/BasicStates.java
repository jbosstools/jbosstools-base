/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.internal.core.parser.rule;

/**
 * 
 * @author V. Kabanovich
 *
 */
public interface BasicStates {
	public int STATE_EXPECTING_EL = 0;
			// 1) "#{"		--> EXPRESSION
	public int STATE_ERROR = -1;


	public int STATE_EXPECTING_EXPRESSION = 101;
			// 1) "}"		--> EL
			// 2) NAME		--> call
			// 3) Primitive, String	--> operation
			// 4) unary, expr		--> operand
			// 5) "["		--> array
	public int STATE_EXPECTING_NAME = 102;  
			// 1) NAME		--> call
	public int STATE_EXPECTING_PARAM = 103;
			// 1) ")"   					--> call after method
			// 2) Primitive, String			--> operation   
			// 3) NAME						--> call
			// 4) unary, expr				--> operand
	public int STATE_EXPECTING_OPERAND = 104; 
			// 1) Primitive, String		 	--> operation   
			// 2) NAME						--> call
			// 4) unary, expr				--> operand

	public int STATE_EXPECTING_OPERATION = 400; 
			// 1) "," 		--> operand
			// 2) ")" 		--> param: call after method
			//					expr:  operation
			// 3) Operation --> operand
			// 4) not in param or array "}"	--> EL
			// 5) "]"		--> array: call after method
	//For all calls: 1) not in params or array "}" --> EL
	//				 2) "." --> name 
	//				 3) in params and array "," --> operand 
	//				 4) in params and array OPERATION --> operand
	//	 			 5) in params ")" --> call after method
	//					in expr   ")" --> operation
	//				 6) in array  "]" --> call after method
	public int STATE_EXPECTING_CALL = 201;  
				  // 5) "(" --> param
				  // 6) "[" --> arg
	public int STATE_EXPECTING_CALL_AFTER_METHOD = 202; //same after arg and array value

	public int STATE_EXPECTING_ARG = 301;          
			// 1) Primitive, String --> arg close

	public int STATE_EXPECTING_ARRAY_VALUE = 303;
			// 1) "]"   					--> call after method
			// 2) Primitive, String			--> operation   
			// 3) NAME						--> call
			// 4) unary, expr				--> operand

}
