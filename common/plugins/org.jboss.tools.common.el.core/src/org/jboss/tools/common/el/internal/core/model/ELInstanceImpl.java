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
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.parser.SyntaxError;
import org.jboss.tools.common.el.internal.core.parser.token.EndELTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.StartELTokenDescription;

/**
 *    '#{' expression '}'
 * @author V. Kabanovich
 */
public class ELInstanceImpl extends ELObjectImpl implements ELInstance {
	ELExpressionImpl expression;
	List<SyntaxError> errors = new ArrayList<SyntaxError>();

	public ELInstanceImpl() {
	}

	public LexicalToken getOpenInstanceToken() {
		LexicalToken result = getFirstToken();
		if(result != null && result.getType() == StartELTokenDescription.START_EL) {
			return result;
		} else {
			return null;
		}
	}

	public LexicalToken getCloseInstanceToken() {
		LexicalToken result = getLastToken();
		if(result != null && result.getType() == EndELTokenDescription.END_EL) {
			return result;
		} else {
			return null;
		}
	}

	public ELExpression getExpression() {
		return expression;
	}

	public List<SyntaxError> getErrors() {
		return errors;
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
		if(getOpenInstanceToken() != null) {
			sb.append(getOpenInstanceToken().getText());
		}
		if(expression != null) {
			sb.append(expression.toString());
		}
		if(getCloseInstanceToken() != null) {
			sb.append(getCloseInstanceToken().getText());
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_INSTANCE;
	}

	public boolean contains(int position) {
		if(position < getFirstToken().getStart()) return false;
		LexicalToken l = getLastToken();
		if(l == null) l = getFirstToken();
		int end = l.getStart() + l.getLength();
		if(position > end) return false;
		if(getCloseInstanceToken() != null && position == end) return false;
		return true;
	}

	public void addError(SyntaxError error) {
		errors.add(error);
	}

}
