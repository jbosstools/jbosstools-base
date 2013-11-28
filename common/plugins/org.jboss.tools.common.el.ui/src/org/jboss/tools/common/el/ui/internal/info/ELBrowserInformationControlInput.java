/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.el.ui.internal.info;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocBrowserInformationControlInput;

/**
 * Browser information control input instance
 * 
 * @author Victor V. Rubezhny
 *
 */
public class ELBrowserInformationControlInput extends JavadocBrowserInformationControlInput {
	private final IJavaElement[] fElements;

	public ELBrowserInformationControlInput(
			JavadocBrowserInformationControlInput previous,
			IJavaElement[] elements, String html, int leadingImageWidth) {
		super(previous, null, html, leadingImageWidth);
		this.fElements = elements;
	}

	/**
	 * Returns the Java elements.
	 *
	 * @return the elements or <code>null</code> if none available
	 */
	public IJavaElement[] getElements() {
		return fElements;
	}
	
	/*
	 * @see org.eclipse.jdt.internal.ui.infoviews.BrowserInput#getInputName()
	 */
	@Override
	public String getInputName() {
		return fElements == null || fElements.length == 0? "" : fElements[0].getElementName(); //$NON-NLS-1$
	}
}
