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

import java.util.ArrayList;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;

public class MarkerAnnotationInfo {
	public final SimpleMarkerAnnotation annotation;
	public final Position position;
	public final ITextViewer viewer;

	public MarkerAnnotationInfo(SimpleMarkerAnnotation annotation, Position position, ITextViewer textViewer) {
		this.annotation= annotation;
		this.position= position;
		this.viewer= textViewer;
	}

	public QuickFixProposal[] getCompletionProposals() {
		ArrayList<QuickFixProposal> proposals = new ArrayList<QuickFixProposal>();
		
		IMarker marker = annotation.getMarker();
		IMarkerResolution[] resolutions = IDE.getMarkerHelpRegistry().getResolutions(marker);
		for (IMarkerResolution resolution : resolutions) {
			proposals.add(new QuickFixProposal(resolution, marker));
		}
		
		return proposals.toArray(new QuickFixProposal[proposals.size()]);
	}
}
