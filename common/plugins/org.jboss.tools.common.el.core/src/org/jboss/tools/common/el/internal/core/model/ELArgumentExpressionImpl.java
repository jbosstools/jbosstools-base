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

import org.jboss.tools.common.el.core.model.ELArgumentInvocation;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELObjectType;

/**
 * 
 * @author V. Kabanovich
 */
public class ELArgumentExpressionImpl extends ELInvocationExpressionImpl implements ELArgumentInvocation {
	ELArgumentImpl argument;

	public ELArgumentExpressionImpl() {}

	public void addChild(ELObjectImpl child) {
	}

	public ELArgumentImpl getArgument() {
		return argument;
	}

	public void setArgument(ELArgumentImpl argument) {
		this.argument = argument;
		if(argument != null) {
			argument.setParent(this);
			setLastToken(argument.getLastToken());
		}
	}

	public String toString() {
		return super.toString() + ((argument != null) ? argument.toString() : ""); //$NON-NLS-1$
	}

	public ELObjectType getType() {
		return ELObjectType.EL_ARGUMENT_INVOCATION;
	}

	public int getInvocationStartPosition() {
		return argument == null ? -1 : argument.getStartPosition();
	}

	public String getMemberName() {
		if(argument == null || argument.getArgument() == null) return null;
		return argument.getArgument().getText();
	}

	public void collectInvocationsInArgument(List<ELInvocationExpression> list) {
		if(argument != null) {
			if(argument.getArgument() != null) {
				argument.getArgument().collectInvocations(list);
			}
		}
	}
}
