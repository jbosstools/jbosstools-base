/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
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
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.internal.core.parser.token.ArrayEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArrayStartTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELArrayImpl extends ELExpressionImpl {
	List<ELExpression> values = new ArrayList<ELExpression>();

	public ELArrayImpl() {}

	public LexicalToken getOpenParametersToken() {
		LexicalToken result = getFirstToken();
		if(result != null && result.getType() == ArrayStartTokenDescription.ARRAY_START) {
			return result;
		} else {
			return null;
		}
	}

	public LexicalToken getCloseParametersToken() {
		LexicalToken result = getLastToken();
		if(result != null && result.getType() == ArrayEndTokenDescription.ARRAY_END) {
			return result;
		} else {
			return null;
		}
	}

	public List<ELExpression> getValues() {
		return values;
	}

	@Override
	public void addChild(ELObjectImpl child) {
		if(child instanceof ELExpressionImpl) {
			addValue((ELExpressionImpl)child);
		} else {
			throw new IllegalArgumentException("EL array can have only EL expressions as its children."); //$NON-NLS-1$
		}
	}

	public void addValue(ELExpressionImpl param) {
		super.addChild(param);
		values.add(param);
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		LexicalToken t = getOpenParametersToken();
		if(t != null) sb.append(t.getText());
		boolean first = true;
		for (ELExpression p: values) {
			if(!first) {
				sb.append(',');
			} else {
				first = false;
			}
			sb.append(p.toString());
			
		}
		t = getCloseParametersToken();
		if(t != null) sb.append(t.getText());
		return sb.toString();
	}

	@Override
	public ELObjectType getType() {
		return ELObjectType.EL_ARRAY;
	}

	@Override
	public void collectInvocations(List<ELInvocationExpression> list) {
		if(values != null) {
			for (ELExpression expr: values) {
				((ELExpressionImpl)expr).collectInvocations(list);
			}
		}
	}
}
