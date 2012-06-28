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
package org.jboss.tools.common.text.xml.quickfix;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jdt.ui.text.java.IInvocationContext;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jdt.ui.text.java.IProblemLocation;
import org.eclipse.jdt.ui.text.java.IQuickAssistProcessor;
import org.eclipse.jdt.ui.text.java.IQuickFixProcessor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class BaseQuickFixProcessor implements IQuickFixProcessor, IQuickAssistProcessor {
	
	public boolean hasAssists(IInvocationContext context) throws CoreException {
		return true;
	}

	public IJavaCompletionProposal[] getAssists(IInvocationContext context,
			IProblemLocation[] locations) throws CoreException {
		
		return getComplitions(locations);
	}

	public boolean hasCorrections(ICompilationUnit unit, int problemId) {
		return true;
	}

	public IJavaCompletionProposal[] getCorrections(IInvocationContext context,
			IProblemLocation[] locations) throws CoreException {
		
		return getComplitions(locations);
	}
	
	private IJavaCompletionProposal[] getComplitions(IProblemLocation[] locations){
		ArrayList<IJavaCompletionProposal> list = new ArrayList<IJavaCompletionProposal>();
		
		Annotation[] annotations = findAnnotations(locations);
		
		String debugInfo = "Annotations: "; //$NON-NLS-1$
		
		for(Annotation annotation : annotations){
			debugInfo += annotation.getText()+" "; //$NON-NLS-1$
			if(QuickFixManager.getInstance().hasProposals(annotation)){
				List<IJavaCompletionProposal> proposals = QuickFixManager.getInstance().getProposals(annotation);
				list.addAll(proposals);
			}
		}
		
		list.add(new BaseCompletionProposal(debugInfo));
		
		return list.toArray(new IJavaCompletionProposal[]{});
	}
	
	private Annotation[] findAnnotations(IProblemLocation[] locations){
		ArrayList<Annotation> annotationList = new ArrayList<Annotation>();
		
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if(window != null){
			IWorkbenchPage page = window.getActivePage();
			if(page != null){
				IEditorPart editor = page.getActiveEditor();
				if(editor instanceof JavaEditor){
					ISourceViewer viwer = ((JavaEditor)editor).getViewer();
					if(viwer != null){
						IAnnotationModel aModel = viwer.getAnnotationModel();
						Iterator iterator = aModel.getAnnotationIterator();
						while(iterator.hasNext()){
							Annotation annotation = (Annotation)iterator.next();
							System.out.println("Annotation - "+annotation.getClass()+" "+annotation.getClass()+" "+annotation.getType());
							Position position = aModel.getPosition(annotation);
							for(IProblemLocation location : locations){
								if(isOverlap(position, location)){
									annotationList.add(annotation);
								}
							}
						}
					}
				}
			}
		}
		
		return annotationList.toArray(new Annotation[]{});
	}
	
	private boolean isOverlap(Position position, IProblemLocation location){
		return isInto(position.getOffset(), position.getLength(), location.getOffset(), location.getLength()) ||
				isInto(location.getOffset(), location.getLength(), position.getOffset(), position.getLength());
	}
	
	private boolean isInto(int offset1, int length1, int offset2, int length2){
		
		return (offset1 >= offset2 && offset1 <= (offset2+length2)) || ((offset1+length2) >= offset2 && (offset1+length2) <= (offset2+length2));
	}
	
	class BaseCompletionProposal implements IJavaCompletionProposal{
		String debugInfo;
		
		public BaseCompletionProposal(String debugInfo){
			this.debugInfo = debugInfo;
		}

		public void apply(IDocument document) {
		}

		public Point getSelection(IDocument document) {
			return null;
		}

		public String getAdditionalProposalInfo() {
			return "Additioonal Proposal Info"; //$NON-NLS-1$
		}

		public String getDisplayString() {
			return "Base Completion Proposal "+debugInfo; //$NON-NLS-1$
		}

		public Image getImage() {
			return null;
		}

		public IContextInformation getContextInformation() {
			return null;
		}

		public int getRelevance() {
			return 100;
		}
		
	}

}
