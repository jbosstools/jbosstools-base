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

import org.jboss.tools.common.el.core.model.ELArgument;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.ArgStartTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELArgumentImpl extends ELObjectImpl implements ELArgument {
	ELExpressionImpl argument;

	public ELArgumentImpl() {}

	public ELExpressionImpl getArgument() {
		return argument;
	}

	public LexicalToken getOpenArgumentToken() {
		LexicalToken result = getFirstToken();
		if(result != null && result.getType() == ArgStartTokenDescription.ARG_START) {
			return result;
		} else {
			return null;
		}
	}

	public LexicalToken getCloseArgumentToken() {
		LexicalToken result = getLastToken();
		if(result != null && result.getType() == ArgEndTokenDescription.ARG_END) {
			return result;
		} else {
			return null;
		}
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELExpressionImpl) {
			setArgument((ELExpressionImpl)child);
		} else {
			throw new IllegalArgumentException("EL argument can have only EL expression as its child.");
		}
	}

	public void setArgument(ELExpressionImpl arg) {
		if(argument == arg) return;
		if(argument != null) {
			removeChild(argument);
		}
		if(arg != null) {
			super.addChild(arg);
		}
		argument = arg;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		if(getOpenArgumentToken() != null) {
			sb.append(getOpenArgumentToken().getText());
		}
		if(argument != null) {
			sb.append(argument.toString());
		}
		if(getCloseArgumentToken() != null) {
			sb.append(getCloseArgumentToken().getText());
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_ARGUMENT;
	}

}
