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
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.quickassist.IQuickFixableAnnotation;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.TextInvocationContext;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;
import org.eclipse.wst.sse.ui.StructuredTextInvocationContext;
import org.eclipse.wst.sse.ui.internal.correction.CompoundQuickAssistProcessor;
import org.eclipse.wst.sse.ui.internal.correction.QuickFixRegistry;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;

public class MarkerAnnotationInfo {
	public final List<AnnotationInfo> infos;
	public final ISourceViewer viewer;
	private CompoundQuickAssistProcessor fCompoundQuickAssistProcessor = new CompoundQuickAssistProcessor();


	public MarkerAnnotationInfo(List<AnnotationInfo> infos, ISourceViewer textViewer) {
		this.infos = infos;
		this.viewer = textViewer;
	}
	
	public List<ICompletionProposal> getCompletionProposals(AnnotationInfo info) {

		if(info.isTop())
			return getMarkerProposals(info);
		else
			return getProposals(info);
		
	}

	public List<ICompletionProposal> getMarkerProposals(AnnotationInfo info) {
		SimpleMarkerAnnotation annotation = (SimpleMarkerAnnotation)info.annotation;
		
		ArrayList<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		
		IMarker marker = annotation.getMarker();
		IMarkerResolution[] resolutions = IDE.getMarkerHelpRegistry().getResolutions(marker);
		for (IMarkerResolution resolution : resolutions) {
			if(!isDirty() || !(resolution instanceof IBaseMarkerResolution)){
				proposals.add(new QuickFixProposal(resolution, marker));
			}
		}
		
		TextInvocationContext sseContext = new TextInvocationContext(viewer, info.position.getOffset(), info.position.getLength());
		
		ICompletionProposal[] compoundQuickAssistProcessorProposals = fCompoundQuickAssistProcessor.computeQuickAssistProposals(sseContext);
		if (compoundQuickAssistProcessorProposals != null) {
			for (ICompletionProposal p : compoundQuickAssistProcessorProposals) {
				proposals.add(p);
			}
		}
		
		return proposals;
	}
	
	public List<ICompletionProposal> getProposals(AnnotationInfo info) {
		TemporaryAnnotation annotation = (TemporaryAnnotation)info.annotation;
		
		List<ICompletionProposal> allProposals = new ArrayList<ICompletionProposal>();
		List<IQuickAssistProcessor> processors = new ArrayList<IQuickAssistProcessor>();
		if (canFix(annotation)) {
			Object o = annotation.getAdditionalFixInfo();
			if (o instanceof IQuickAssistProcessor) {
				processors.add((IQuickAssistProcessor)o);
			}
			
			// get all relevant quick fixes for this annotation
			
			if(isDirty() && QuickFixManager.getInstance().hasProposals(annotation, info.position)){
				annotation.setAdditionalFixInfo(viewer.getDocument());
				List<IJavaCompletionProposal> proposals = QuickFixManager.getInstance().getProposals(annotation, info.position);
				allProposals.addAll(proposals);
			}

			QuickFixRegistry registry = QuickFixRegistry.getInstance();
			processors.addAll(Arrays.asList(registry.getQuickFixProcessors(annotation)));

			// set up context
			Map attributes = annotation.getAttributes();
			StructuredTextInvocationContext sseContext = new StructuredTextInvocationContext(viewer, info.position.getOffset(), info.position.getLength(), attributes);
			
			ICompletionProposal[] compoundQuickAssistProcessorProposals = fCompoundQuickAssistProcessor.computeQuickAssistProposals(sseContext);
			if (compoundQuickAssistProcessorProposals != null) {
				for (ICompletionProposal p : compoundQuickAssistProcessorProposals) {
					allProposals.add(p);
				}
			}

			// call each processor
			for (int i = 0; i < processors.size(); ++i) {
				List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
				collectProposals((IQuickAssistProcessor) processors.get(i), annotation, sseContext, proposals);

				if (proposals.size() > 0) {
					allProposals.addAll(proposals);
				}
			}

		}

		return allProposals;
	}
	
	private static boolean isDirty(){
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if(window != null){
			IWorkbenchPage page = window.getActivePage();
			if(page != null){
				IEditorPart editor = page.getActiveEditor();
				if(editor != null){
					return editor.isDirty();
				}
			}
		}
		return true;
	}
	
	private void collectProposals(IQuickAssistProcessor processor, Annotation annotation, IQuickAssistInvocationContext invocationContext, List<ICompletionProposal> proposalsList) {
		ICompletionProposal[] proposals = processor.computeQuickAssistProposals(invocationContext);
		if (proposals != null && proposals.length > 0) {
			proposalsList.addAll(Arrays.asList(proposals));
		}
	}
	
	public boolean canFix(Annotation annotation) {
		if (annotation instanceof IQuickFixableAnnotation) {
			if (((IQuickFixableAnnotation) annotation).isQuickFixableStateSet()) {
				return ((IQuickFixableAnnotation) annotation).isQuickFixable();
			}
		}else if(annotation instanceof TemporaryAnnotation){
			if (((TemporaryAnnotation) annotation).isQuickFixableStateSet()) {
				return ((TemporaryAnnotation) annotation).isQuickFixable();
			}
		}
		return false;
	}
	
	public static class AnnotationInfo {
		public Annotation annotation;
		public Position position;
		
		public AnnotationInfo(Annotation annotation, Position position){
			this.annotation = annotation;
			this.position = position;
		}
		
		public boolean isTop(){
			return annotation instanceof SimpleMarkerAnnotation;
		}
	}

	@Override
	public String toString() {
		return null;
	}
}