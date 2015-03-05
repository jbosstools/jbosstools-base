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
package org.jboss.tools.common.quickfix;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.TextInvocationContext;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;
import org.eclipse.wst.sse.ui.StructuredTextInvocationContext;
import org.eclipse.wst.sse.ui.internal.correction.CompoundQuickAssistProcessor;
import org.eclipse.wst.sse.ui.internal.correction.QuickFixRegistry;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.eclipse.wst.xml.ui.internal.XMLUIMessages;
import org.jboss.tools.common.CommonPlugin;

public class MarkerAnnotationInfo {
	public static final String UNKNOWN_TAG = "Unknown tag"; //$NON-NLS-1$
	public static final String MISSING_ATTRIBUTE = "Missing required attribute"; //$NON-NLS-1$
	public static final String PREFERENCE_KEY_ATTRIBUTE_NAME = "preference_key"; //$NON-NLS-1$
	public static final String MESSAGE_TYPE_ATTRIBUTE_NAME = "jbt.type"; //$NON-NLS-1$

	public final List<AnnotationInfo> infos;
	public final ISourceViewer viewer;
	private CompoundQuickAssistProcessor fCompoundQuickAssistProcessor = new CompoundQuickAssistProcessor();


	public MarkerAnnotationInfo(List<AnnotationInfo> infos, ISourceViewer textViewer) {
		this.infos = infos;
		this.viewer = textViewer;
	}
	
	public List<ICompletionProposal> getCompletionProposals(AnnotationInfo info) {
		ArrayList<String> proposalNames = new ArrayList<String>();
		proposalNames.add(XMLUIMessages.SurroundWithNewElementQuickAssistProposal_1);
		ArrayList<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		for(Annotation annotation : info.getAnnotations()){
			if(annotation instanceof SimpleMarkerAnnotation){
				for (ICompletionProposal proposal : getMarkerProposals((SimpleMarkerAnnotation)annotation, info.getPosition())) {
					if(!proposalNames.contains(proposal.getDisplayString())){
						proposals.add(proposal);
						proposalNames.add(proposal.getDisplayString());
					}
				}
			} else if(annotation instanceof TemporaryAnnotation){
				for (ICompletionProposal proposal : getProposals((TemporaryAnnotation)annotation, info.getPosition())) {
					if(!proposalNames.contains(proposal.getDisplayString())){
						proposals.add(proposal);
						proposalNames.add(proposal.getDisplayString());
					}
				}
			}
		}
		return proposals;
	}
	
	private static boolean isJBTAnnotation(Annotation annotation){
		boolean isJBTAnnotation = false;
		if(annotation instanceof SimpleMarkerAnnotation){
			SimpleMarkerAnnotation sa = (SimpleMarkerAnnotation)annotation;
			if(sa.getMarker() != null && sa.getMarker().exists()){
				try {
					isJBTAnnotation = sa.getMarker().getAttribute(PREFERENCE_KEY_ATTRIBUTE_NAME) != null;
				} catch (CoreException e) {
					CommonPlugin.getDefault().logError(e);
				}
			}
		}else if(annotation instanceof TemporaryAnnotation){
			TemporaryAnnotation ta = (TemporaryAnnotation)annotation;
			if(ta.getAttributes() != null){
				String attribute = (String)ta.getAttributes().get(MESSAGE_TYPE_ATTRIBUTE_NAME);
				isJBTAnnotation = attribute != null;
			}
		}
		return isJBTAnnotation;
	}

	private List<ICompletionProposal> getMarkerProposals(SimpleMarkerAnnotation annotation, Position position) {
		ArrayList<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		
		boolean isJBTAnnotation = isJBTAnnotation(annotation);
		
		IMarker marker = annotation.getMarker();
		IMarkerResolution[] resolutions = IDE.getMarkerHelpRegistry().getResolutions(marker);
		for (IMarkerResolution resolution : resolutions) {
			proposals.add(new QuickFixProposal(resolution, marker));
		}
		if(!isJBTAnnotation){
			TextInvocationContext sseContext = new TextInvocationContext(viewer, position.getOffset(), position.getLength());
			
			ICompletionProposal[] compoundQuickAssistProcessorProposals = fCompoundQuickAssistProcessor.computeQuickAssistProposals(sseContext);
			if (compoundQuickAssistProcessorProposals != null) {
				for (ICompletionProposal proposal : compoundQuickAssistProcessorProposals) {
						proposals.add(proposal);
				}
			}
		}
		return proposals;
	}
	
	private List<ICompletionProposal> getProposals(TemporaryAnnotation annotation, Position position) {
		List<ICompletionProposal> allProposals = new ArrayList<ICompletionProposal>();
		List<IQuickAssistProcessor> processors = new ArrayList<IQuickAssistProcessor>();
		
		boolean isJBTAnnotation = isJBTAnnotation(annotation);
		
		// get all relevant quick fixes for this annotation
		if(QuickFixManager.getInstance().hasProposals(annotation, position)){
			if(annotation.getText().startsWith(UNKNOWN_TAG) || annotation.getText().startsWith(MISSING_ATTRIBUTE)){
				annotation.setAdditionalFixInfo(viewer.getDocument());
			}
			List<IJavaCompletionProposal> proposals = QuickFixManager.getInstance().getProposals(annotation, position);
			allProposals.addAll(proposals);
		}
		
		if(!isJBTAnnotation){
			Object o = annotation.getAdditionalFixInfo();
			if (o instanceof IQuickAssistProcessor) {
				processors.add((IQuickAssistProcessor)o);
			}
			QuickFixRegistry registry = QuickFixRegistry.getInstance();
			processors.addAll(Arrays.asList(registry.getQuickFixProcessors(annotation)));
		
			// set up context
			Map attributes = annotation.getAttributes();
			StructuredTextInvocationContext sseContext = new StructuredTextInvocationContext(viewer, position.getOffset(), position.getLength(), attributes);
				
			ICompletionProposal[] compoundQuickAssistProcessorProposals = fCompoundQuickAssistProcessor.computeQuickAssistProposals(sseContext);
			if (compoundQuickAssistProcessorProposals != null) {
				for (ICompletionProposal proposal : compoundQuickAssistProcessorProposals) {
					allProposals.add(proposal);
				}
			}
			
			// call each processor
			for (int i = 0; i < processors.size(); ++i) {
				List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
				collectProposals((IQuickAssistProcessor) processors.get(i), annotation, sseContext, proposals);
				allProposals.addAll(proposals);
			}
		}

		return allProposals;
	}
	
	private void collectProposals(IQuickAssistProcessor processor, Annotation annotation, IQuickAssistInvocationContext invocationContext, List<ICompletionProposal> proposalsList) {
		ICompletionProposal[] proposals = processor.computeQuickAssistProposals(invocationContext);
		if (proposals != null && proposals.length > 0) {
			proposalsList.addAll(Arrays.asList(proposals));
		}
	}
	
	public boolean canFix(Annotation annotation) {
		return true;
	}
	
	public static class AnnotationInfo {
		private ArrayList<Annotation> annotations = new ArrayList<Annotation>();
		private Position position;
		
		public AnnotationInfo(Annotation annotation, Position position){
			add(annotation);
			this.position = position;
		}
		
		public void add(Annotation annotation){
			annotations.add(annotation);
		}
		
		public List<Annotation> getAnnotations(){
			return annotations;
		}
		
		public Annotation getMainAnnotation(){
			for(Annotation annotation : annotations){
				if(annotation instanceof SimpleMarkerAnnotation){
					return annotation;
				}
			}
			return annotations.get(0);
		}
		
		public Position getPosition(){
			return position;
		}
		
		public boolean isTop(){
			return isJBTAnnotation(getMainAnnotation());
		}
	}

	@Override
	public String toString() {
		return null;
	}
}