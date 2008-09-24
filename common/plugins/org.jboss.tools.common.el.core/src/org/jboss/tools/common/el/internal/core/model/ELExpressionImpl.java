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

/**
 * 
 * @author V. Kabanovich
 *
 */
public abstract class ELExpressionImpl extends ELObjectImpl implements ELExpression {

	public ELExpressionImpl() {}

	public List<ELInvocationExpression> getInvocations() {
		List<ELInvocationExpression> list = new ArrayList<ELInvocationExpression>();
		collectInvocations(list);
		return list;
	}

	public void collectInvocations(List<ELInvocationExpression> list) {
	}

}
