/******************************************************************************* 
 * Copyright (c) 2011-2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.text.xml;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;
import org.eclipse.wst.sse.ui.internal.correction.CompoundQuickAssistProcessor;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.jboss.tools.common.quickfix.MarkerAnnotationInfo;
import org.jboss.tools.common.quickfix.MarkerAnnotationInfo.AnnotationInfo;

public class BaseQuickAssistProcessor implements IQuickAssistProcessor {
	private CompoundQuickAssistProcessor fCompoundQuickAssistProcessor = new CompoundQuickAssistProcessor();

	public String getErrorMessage() {
		return null;
	}

	public boolean canFix(Annotation annotation) {
		if(internalCanFix(annotation))
			return true;
		
		return fCompoundQuickAssistProcessor.canFix(annotation);
	}
	
	private boolean internalCanFix(Annotation annotation) {
		return (annotation instanceof SimpleMarkerAnnotation || annotation instanceof TemporaryAnnotation);
	}

	public boolean canAssist(IQuickAssistInvocationContext invocationContext) {
		return fCompoundQuickAssistProcessor.canAssist(invocationContext);
	}

	public ICompletionProposal[] computeQuickAssistProposals(IQuickAssistInvocationContext invocationContext) {
		List<AnnotationInfo> all = new ArrayList<AnnotationInfo>();
		List<AnnotationInfo> high = new ArrayList<AnnotationInfo>();
		List<AnnotationInfo> low = new ArrayList<AnnotationInfo>();
		
		IAnnotationModel model = invocationContext.getSourceViewer().getAnnotationModel();
		if (model != null) {
			Iterator<Annotation> iterator = model.getAnnotationIterator();
			while (iterator.hasNext()) {
				Annotation annotation = (Annotation) iterator.next();
				if (!internalCanFix(annotation))
					continue;

				Position position = model.getPosition(annotation);
				
				if (position.overlapsWith(invocationContext.getOffset(), 1)) {
					AnnotationInfo info = new AnnotationInfo(annotation, position);
					if(info.isTop())
						high.add(info);
					else
						low.add(info);
				}
			}
			all.addAll(high);
			all.addAll(low);
		}
		MarkerAnnotationInfo mai = new MarkerAnnotationInfo(all, invocationContext.getSourceViewer());
		List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		for(AnnotationInfo info : all){
			proposals.addAll(mai.getCompletionProposals(info));
		}
		ICompletionProposal[] compoundQuickAssistProcessorProposals = fCompoundQuickAssistProcessor.computeQuickAssistProposals(invocationContext);
		if (compoundQuickAssistProcessorProposals != null) {
			for (ICompletionProposal p : compoundQuickAssistProcessorProposals) {
				proposals.add(p);
			}
		}
		return proposals.toArray(new ICompletionProposal[]{});
	}
}
