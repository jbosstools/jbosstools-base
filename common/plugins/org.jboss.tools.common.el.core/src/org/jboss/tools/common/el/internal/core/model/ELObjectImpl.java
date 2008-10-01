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

import org.jboss.tools.common.el.core.model.ELObject;
import org.jboss.tools.common.el.core.parser.LexicalToken;

/**
 * 
 * @author V. Kabanovich
 *
 */
public abstract class ELObjectImpl implements ELObject {
	protected LexicalToken firstToken;
	protected LexicalToken lastToken;
	protected ELObjectImpl parent;
	protected List<ELObject> children = new ArrayList<ELObject>();

	public ELObjectImpl() {
	}

	public ELModelImpl getModel() {
		return parent == null ? null : parent.getModel();
	}

	public int getLength() {
		return getEndPosition() - getStartPosition();
	}

	public int getStartPosition() {
		return firstToken == null ? -1 : firstToken.getStart();
	}
	public int getEndPosition() {
		LexicalToken lt = (lastToken != null) ? lastToken : firstToken;
		return lt == null ? -1 : lt.getStart() + lt.getLength();
	}

	public String getText() {
		if(getModel() == null) return null;
		String source = getModel().getSource();
		int start = firstToken.getStart() - getModel().delta;
		LexicalToken lt = (lastToken != null) ? lastToken : firstToken;
		int end = lt.getStart() + lt.getLength() - getModel().delta;
		return source.substring(start, end);
	}

	public ELObjectImpl getParent() {
		return parent;
	}

	public List<ELObject> getChildren() {
		return children;
	}

	public LexicalToken getFirstToken() {
		return firstToken;
	}

	public LexicalToken getLastToken() {
		return lastToken;
	}

	public void setParent(ELObjectImpl parent) {
		this.parent = parent;
	}

	public void addChild(ELObjectImpl child) {
		children.add(child);
		child.setParent(this);
	}

	protected void removeChild(ELObjectImpl child) {
		if(children.contains(child)) {
			children.remove(child);
			child.setParent(null);
		}
	}

	public void setFirstToken(LexicalToken firstToken) {
		this.firstToken = firstToken;
	}

	public void setLastToken(LexicalToken lastToken) {
		this.lastToken = lastToken;
	}

}
