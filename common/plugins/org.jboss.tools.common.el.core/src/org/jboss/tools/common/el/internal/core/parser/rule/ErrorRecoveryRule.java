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

import org.jboss.tools.common.el.core.parser.IRule;
import org.jboss.tools.common.el.core.parser.Tokenizer;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.EndELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.StartELTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ErrorRecoveryRule implements IRule {

	public static ErrorRecoveryRule INSTANCE = new ErrorRecoveryRule();

	public int getFinalState(int state, int token) {
		switch (token) {
			case EndELTokenDescription.END_EL:
				return BasicStates.STATE_EXPECTING_EL;
			case StartELTokenDescription.START_EL:
				return BasicStates.STATE_EXPECTING_EXPRESSION;
			case ArgEndTokenDescription.ARG_END:
			case ParamEndTokenDescription.PARAM_END:
				return BasicStates.STATE_EXPECTING_CALL_AFTER_METHOD;
			case ExprEndTokenDescription.EXPR_END:
				return BasicStates.STATE_EXPECTING_OPERATION;
		}
		return 0;
	}

	public int[] getStartStates() {
		return new int[]{BasicStates.STATE_ERROR};
	}

	public int[] getTokenTypes(int state) {
		return new int[]{
			ArgEndTokenDescription.ARG_END,
			ParamEndTokenDescription.PARAM_END,
			ExprEndTokenDescription.EXPR_END,
			EndELTokenDescription.END_EL,
			StartELTokenDescription.START_EL
		};
	}

	public String getProblem(int state, Tokenizer tokenizer) {
		// Other rules already reported a problem.
		return null;
	}

}
