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
import org.jboss.tools.common.el.core.model.ELObject;

/**
 * 
 * @author V. Kabanovich
 *
 */
public abstract class ELInvocationExpressionImpl extends ELExpressionImpl implements ELInvocationExpression {
	protected ELInvocationExpressionImpl left;

	public ELInvocationExpressionImpl() {}

	public void addChild(ELObjectImpl child) {
	}

	public ELInvocationExpressionImpl getLeft() {
		return left;
	}

	public void setLeft(ELInvocationExpressionImpl left) {
		this.left = left;
		if(left != null) {
			left.setParent(this);
			setFirstToken(left.getFirstToken());
			ELObject p = parent;
			while(p instanceof ELInvocationExpressionImpl) {
				((ELInvocationExpressionImpl)p).setFirstToken(firstToken);
				p = p.getParent();
			}
		}
	}

	public String toString() {
		return left != null ? left.toString() : ""; //$NON-NLS-1$
	}

	public void collectInvocations(List<ELInvocationExpression> list) {
		list.add(this);
		//We do not need left part expression, it is part of this invocation
		ELInvocationExpressionImpl l = this;
		while(l != null) {
			if(l instanceof ELMethodInvocationImpl) {
				((ELMethodInvocationImpl)l).collectInvocationsInParameters(list);
			} else if(l instanceof ELArgumentExpressionImpl) {
				((ELArgumentExpressionImpl)l).collectInvocationsInArgument(list);
			}
			l = l.getLeft();
		}
	}
}
