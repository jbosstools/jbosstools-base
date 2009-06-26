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

import org.eclipse.jface.text.TypedRegion;

public class HyperlinkRegion extends TypedRegion implements IHyperlinkRegion {
	String axis;
	String contentType;
	
	public HyperlinkRegion(int propStart, int propLength) {
		this(propStart, propLength, null, null, null);
	}

	public HyperlinkRegion(int start, int length, String axis, String contentType, String type) {
		super(start,length,type);
		this.axis = axis;
		this.contentType = contentType;
	}

	public String getAxis() {
		return axis;
	}

	public String getContentType() {
		return contentType;
	}
	
	public boolean equals(Object arg) {
		if (!(arg instanceof IHyperlinkRegion)) return false;
		IHyperlinkRegion region = (IHyperlinkRegion)arg;
		return getOffset() == region.getOffset() 
			&& getLength() == region.getLength()
			&& areArqumentsEqual(getContentType(), region.getContentType())
			&& areArqumentsEqual(getAxis(), region.getAxis());
	}
	
	private boolean areArqumentsEqual(String s1, String s2) {
		return (s1 == null) ? s2 == null : s1.equals(s2);
	}

	public String toString() {
		return "IHyperlinkRegion [" + getOffset() +", " + getLength()+ "]:[[" + getContentType() + "]/[" + getType() + "]" + getAxis(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
	}

}
