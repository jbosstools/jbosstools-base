/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

public class HyperlinkRegion implements IHyperlinkRegion {
	int start;
	int length;
	
	String axis;
	String contentType;
	String type;
	
	public HyperlinkRegion(int propStart, int propLength) {
		this(propStart, propLength, null, null, null);
	}

	public HyperlinkRegion(int start, int length, String axis, String contentType, String type) {
		this.start = start;
		this.length = length;
		this.axis = axis;
		this.contentType = contentType;
		this.type = type;
	}

	public String getAxis() {
		return axis;
	}

	public String getContentType() {
		return contentType;
	}

	public String getType() {
		return type;
	}

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return start;
	}

	public boolean equals(Object arg) {
		if (!(arg instanceof IHyperlinkRegion)) return false;
		IHyperlinkRegion region = (IHyperlinkRegion)arg;
		if (getOffset() != region.getOffset()) return false;
		if (getLength() != region.getLength()) return false;
		if(!areArqumentsEqual(getContentType(), region.getContentType())) return false;
		if(!areArqumentsEqual(getType(), region.getType())) return false;
		if(!areArqumentsEqual(getAxis(), region.getAxis())) return false;
		return true;
	}
	
	private boolean areArqumentsEqual(String s1, String s2) {
		return (s1 == null) ? s2 == null : s1.equals(s2);
	}

	public String toString() {
		return "IHyperlinkRegion [" + getOffset() +", " + getLength()+ "]:[[" + getContentType() + "]/[" + getType() + "]" + getAxis();
	}

}
