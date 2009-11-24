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

import org.jboss.tools.common.el.core.ElCoreMessages;
import org.jboss.tools.common.el.core.parser.IRule;
import org.jboss.tools.common.el.core.parser.Tokenizer;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.CommaTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.EndELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.OperationTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamUtil;
import org.jboss.tools.common.el.internal.core.parser.token.WhiteSpaceTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class OperationRule implements IRule, BasicStates {

	public static OperationRule INSTANCE = new OperationRule();

	public int[] getStartStates() {
		return new int[] {
			STATE_EXPECTING_OPERATION,
		};
	}

	public int getFinalState(int state, int token) {
		switch (token) {
			case WhiteSpaceTokenDescription.WHITESPACE: 
					return state;
			case EndELTokenDescription.END_EL:
					return STATE_EXPECTING_EL;
			case CommaTokenDescription.COMMA:
			case OperationTokenDescription.OPERATION:
					return STATE_EXPECTING_OPERAND;
			case ParamEndTokenDescription.PARAM_END:
			case ArgEndTokenDescription.ARG_END:
					return STATE_EXPECTING_CALL_AFTER_METHOD;
			case ExprEndTokenDescription.EXPR_END:
					return STATE_EXPECTING_OPERATION;
		}

		return 0;
	}

	public int[] getTokenTypes(int state) {
		if(state == STATE_EXPECTING_OPERATION) {
			return new int[] {
				WhiteSpaceTokenDescription.WHITESPACE,
				ParamEndTokenDescription.PARAM_END,
				ArgEndTokenDescription.ARG_END,
				ExprEndTokenDescription.EXPR_END,
				CommaTokenDescription.COMMA,
				OperationTokenDescription.OPERATION,
				EndELTokenDescription.END_EL,
			};
		}
		return new int[0];
	}

	public String getProblem(int state, Tokenizer tokenizer) {
		if(ParamUtil.isMethodParamContext(tokenizer.getContext())) {
			return ElCoreMessages.OperationRule_ExpectingRParen;
		} else if(ParamUtil.isComplexExpressionContext(tokenizer.getContext())) {
			return ElCoreMessages.OperationRule_ExpectingRParen;
		} else if(ParamUtil.isArgContext(tokenizer.getContext())) {
			return ElCoreMessages.OperationRule_ExpectingRBracket;
		}
		return ElCoreMessages.OperationRule_ExpectingRBrace;
	}

}
