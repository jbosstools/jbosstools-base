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
package org.jboss.tools.common.el.internal.core.model;

import java.util.List;

import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.internal.core.parser.token.ExprEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ExprStartTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.UnaryTokenDescription;

/**
 *    '(' expression ')'
 * @author V. Kabanovich
 */
public class ELComplexExpressionImpl extends ELExpressionImpl {
	ELExpressionImpl expression;

	public ELComplexExpressionImpl() {
	}

	public ELExpressionImpl getExpression() {
		return expression;
	}

	public LexicalToken getOpenExpressionToken() {
		LexicalToken result = getFirstToken();
		if(result != null && 
			(result.getType() == ExprStartTokenDescription.EXPR_START
					|| result.getType() == UnaryTokenDescription.UNARY)) {
			return result;
		} else {
			return null;
		}
	}

	public LexicalToken getCloseExpressionToken() {
		LexicalToken result = getLastToken();
		if(result != null && result.getType() == ExprEndTokenDescription.EXPR_END) {
			return result;
		} else {
			return null;
		}
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELExpressionImpl) {
			setExpression((ELExpressionImpl)child);
		} else {
			throw new IllegalArgumentException("EL instance can have only EL expression as child.");
		}
	}

	public void setExpression(ELExpressionImpl expression) {
		if(this.expression == expression) {
			return;
		}
		if(this.expression != null) {
			removeChild(this.expression);
		}
		this.expression = expression;
		if(expression != null) {
			super.addChild(expression);
		}
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		if(getOpenExpressionToken() != null) {
			sb.append(getOpenExpressionToken().getText());
		}
		if(expression != null) {
			sb.append(expression.toString());
		}
		if(getCloseExpressionToken() != null) {
			sb.append(getCloseExpressionToken().getText());
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_COMPLEX_EXPRESSION;
	}

	public void collectInvocations(List<ELInvocationExpression> list) {
		if(expression != null) {
			expression.collectInvocations(list);
		}
	}
}
