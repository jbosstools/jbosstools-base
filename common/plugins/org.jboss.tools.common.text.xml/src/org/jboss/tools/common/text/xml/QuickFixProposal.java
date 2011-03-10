/*******************************************************************************
 * Copyright (c) 2011 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.xml;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

public class QuickFixProposal implements ICompletionProposal{
	private IMarkerResolution resolution;
	private IMarker marker;
	
	public QuickFixProposal(IMarkerResolution resolution, IMarker marker){
		this.resolution = resolution;
		this.marker = marker;
	}

	public void apply(IDocument document) {
		resolution.run(marker);
	}

	public Point getSelection(IDocument document) {
		return null;
	}

	public String getAdditionalProposalInfo() {
		if (resolution instanceof IMarkerResolution2)
			return ((IMarkerResolution2) resolution).getDescription();
		
		return (String)marker.getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
	}

	public String getDisplayString() {
		return resolution.getLabel();
	}

	public Image getImage() {
		if (resolution instanceof IMarkerResolution2)
			return ((IMarkerResolution2)resolution).getImage();
		return null;
	}

	public IContextInformation getContextInformation() {
		return null;
	}
}
