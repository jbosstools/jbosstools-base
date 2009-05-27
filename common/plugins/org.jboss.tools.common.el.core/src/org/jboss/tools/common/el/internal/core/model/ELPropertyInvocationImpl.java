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

import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
import org.jboss.tools.common.el.core.parser.LexicalToken;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELPropertyInvocationImpl extends ELInvocationExpressionImpl implements ELPropertyInvocation {
	LexicalToken dot;
	LexicalToken name;

	public ELPropertyInvocationImpl() {}

	public void addChild(ELObjectImpl child) {
	}

	public LexicalToken getSeparator() {
		return dot;
	}

	public LexicalToken getName() {
		return name;
	}

	public void setName(LexicalToken name) {
		this.name = name;
		if(name != null) {
			if(left == null) setFirstToken(name);
			setLastToken(name);
		}
	}

	public void setSeparator(LexicalToken separator) {
		dot = separator;
	}

	public String getQualifiedName() {
		if(name == null) return null;
		if(left == null) return name.getText();
		if(!(left instanceof ELPropertyInvocationImpl)) {
			return null;
		}
		String p = ((ELPropertyInvocationImpl)left).getQualifiedName();
		if(p == null) return null;
		return p + "." + name.getText(); //$NON-NLS-1$
	}

	public String toString() {
		return super.toString() + (dot != null ? dot.getText() : "") //$NON-NLS-1$
			+ (name != null ? name.getText() : ""); //$NON-NLS-1$
	}

	public ELObjectType getType() {
		return ELObjectType.EL_PROPERTY_INVOCATION;
	}

	public int getInvocationStartPosition() {
		if(name != null) {
			return name.getStart();
		}
		if(dot != null) {
			return dot.getStart(); // ?
		}
		return -1;
	}

	public String getMemberName() {
		return name == null ? null : name.getText();
	}

}
