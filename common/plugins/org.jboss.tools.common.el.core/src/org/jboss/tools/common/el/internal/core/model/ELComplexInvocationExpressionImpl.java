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
public class ELComplexInvocationExpressionImpl extends ELExpressionImpl {
	ELComplexExpressionImpl expression;
	ELInvocationExpressionImpl invocation;

	public ELComplexInvocationExpressionImpl() {
	}

	public ELExpressionImpl getExpression() {
		return expression;
	}

	public ELInvocationExpressionImpl getInvocation() {
		return invocation;
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELComplexExpressionImpl) {
			setExpression((ELComplexExpressionImpl)child);
		} else if(child instanceof ELInvocationExpressionImpl) {
			setInvocation((ELInvocationExpressionImpl)child);
		} else {
			throw new IllegalArgumentException("EL instance can have only EL expression as child."); //$NON-NLS-1$
		}
	}

	public void setExpression(ELComplexExpressionImpl expression) {
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

	public void setInvocation(ELInvocationExpressionImpl invocation) {
		if(this.invocation == invocation) {
			return;
		}
		if(this.invocation != null) {
			removeChild(this.invocation);
		}
		this.invocation = invocation;
		if(invocation != null) {
			super.addChild(invocation);
		}
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		if(expression != null) {
			sb.append(expression.toString());
		}
		if(invocation != null) {
			sb.append(invocation.toString());
			
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_METHOD_INVOCATION;
	}

	public void collectInvocations(List<ELInvocationExpression> list) {
		if(expression != null) {
			expression.collectInvocations(list);
		}
		if(invocation != null) {
			list.add(invocation);
		}
	}
}
