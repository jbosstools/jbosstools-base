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
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.model.ELParameters;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.internal.core.parser.token.ParamEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ParamStartTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELParametersImpl extends ELObjectImpl implements ELParameters {
	List<ELExpression> parameters = new ArrayList<ELExpression>();

	public ELParametersImpl() {}

	public LexicalToken getOpenParametersToken() {
		LexicalToken result = getFirstToken();
		if(result != null && result.getType() == ParamStartTokenDescription.PARAM_START) {
			return result;
		} else {
			return null;
		}
	}

	public LexicalToken getCloseParametersToken() {
		LexicalToken result = getLastToken();
		if(result != null && result.getType() == ParamEndTokenDescription.PARAM_END) {
			return result;
		} else {
			return null;
		}
	}

	public List<ELExpression> getParameters() {
		return parameters;
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELExpressionImpl) {
			addParameter((ELExpressionImpl)child);
		} else {
			throw new IllegalArgumentException("EL parameters can have only EL expressions as its children."); //$NON-NLS-1$
		}
	}

	public void addParameter(ELExpressionImpl param) {
		super.addChild(param);
		parameters.add(param);
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		LexicalToken t = getOpenParametersToken();
		if(t != null) sb.append(t.getText());
		boolean first = true;
		for (ELExpression p: parameters) {
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

	public ELObjectType getType() {
		return ELObjectType.EL_PARAMETERS;
	}

}
