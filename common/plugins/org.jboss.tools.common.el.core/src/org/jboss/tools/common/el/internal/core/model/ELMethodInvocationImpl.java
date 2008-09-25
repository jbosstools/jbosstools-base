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

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELObjectType;

/**
 *    LEFT . name parameters
 * @author V. Kabanovich
 */
public class ELMethodInvocationImpl extends ELPropertyInvocationImpl implements ELMethodInvocation {
	ELParametersImpl parameters;

	public ELMethodInvocationImpl() {}

	public void addChild(ELObjectImpl child) {
	}

	public ELParametersImpl getParameters() {
		return parameters;
	}

	public void setParameters(ELParametersImpl parameters) {
		this.parameters = parameters;
		if(parameters != null) {
			parameters.setParent(this);
			setLastToken(parameters.getLastToken());
		}
	}

	public String getQualifiedName() {
		return null;
	}

	public String toString() {
		return super.toString() + ((parameters != null) ? parameters.toString() : "");
	}

	public ELObjectType getType() {
		return ELObjectType.EL_METHOD_INVOCATION;
	}

	public void collectInvocationsInParameters(List<ELInvocationExpression> list) {
		if(parameters != null) {
			List<ELExpression> ps = parameters.getParameters();
			for (ELExpression expr: ps) {
				((ELExpressionImpl)expr).collectInvocations(list);
			}
		}
	}
}
