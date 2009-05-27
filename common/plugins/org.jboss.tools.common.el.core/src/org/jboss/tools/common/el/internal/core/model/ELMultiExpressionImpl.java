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

import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMultiExpression;
import org.jboss.tools.common.el.core.model.ELObject;
import org.jboss.tools.common.el.core.model.ELObjectType;

/**
 *    expression (operation expression)+
 * @author V. Kabanovich
 */
public class ELMultiExpressionImpl extends ELExpressionImpl implements ELMultiExpression {
	List<ELExpression> expressions = new ArrayList<ELExpression>();
	List<ELOperatorImpl> operators = new ArrayList<ELOperatorImpl>();

	public ELMultiExpressionImpl() {
	}

	public List<ELExpression> getExpressions() {
		return expressions;
	}

	public List<ELOperatorImpl> getOperators() {
		return operators;
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELExpressionImpl) {
			addExpression((ELExpressionImpl)child);
		} else if(child instanceof ELOperatorImpl) {
			addOperator((ELOperatorImpl)child);
		} else {
			throw new IllegalArgumentException("EL instance can have only EL expression as child."); //$NON-NLS-1$
		}
	}

	public void addExpression(ELExpressionImpl expression) {
		if(expressions.size() > operators.size()) {
			throw new IllegalArgumentException("Expecting operator"); //$NON-NLS-1$
		}
		super.addChild(expression);
		expressions.add(expression);
	}

	public void addOperator(ELOperatorImpl operator) {
		while(expressions.size() <= operators.size()) {
			addChild(new ELExpressionImpl() {
				public ELObjectType getType() {
					return ELObjectType.EL_UNKNOWN;
				}
			});
		}
		super.addChild(operator);
		operators.add(operator);
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		if(children != null) for (ELObject c: children) {
			sb.append(c.toString());
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_MULTI_EXPRESSION;
	}

	public void collectInvocations(List<ELInvocationExpression> list) {
		for (ELExpression expr: expressions) {
			((ELExpressionImpl)expr).collectInvocations(list);
		}
	}

}
