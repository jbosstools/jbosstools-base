/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui.quickfix;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.internal.ui.text.java.hover.AbstractAnnotationHover;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.source.Annotation;
import org.jboss.tools.common.quickfix.QuickFixManager;

@SuppressWarnings("restriction")
public class BaseJavaHover extends AbstractAnnotationHover{
	public BaseJavaHover() {
		super(true);
	}
	
	@Override
	protected AnnotationInfo createAnnotationInfo(Annotation annotation, Position position, ITextViewer textViewer) {
		AnnotationInfo info = new ProblemInfo(annotation, position, textViewer);
		if(info.getCompletionProposals().length > 0){
			return info;
		}
		return null;
	}
	
	protected static class ProblemInfo extends AnnotationInfo {

		public ProblemInfo(Annotation annotation, Position position, ITextViewer textViewer) {
			super(annotation, position, textViewer);
		}

		/*
		 * @see org.eclipse.jdt.internal.ui.text.java.hover.AbstractAnnotationHover.AnnotationInfo#getCompletionProposals()
		 */
		@Override
		public ICompletionProposal[] getCompletionProposals() {
			ArrayList<IJavaCompletionProposal> proposals= new ArrayList<IJavaCompletionProposal>();
			
			if(QuickFixManager.getInstance().hasProposals(annotation)){
				List<IJavaCompletionProposal> pp = QuickFixManager.getInstance().getProposals(annotation);
				proposals.addAll(pp);
			}
			return proposals.toArray(new ICompletionProposal[proposals.size()]);
		}
	}
}
