/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.refactoring;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.ltk.core.refactoring.TextChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.swt.graphics.Point;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.quickfix.IQuickFix;

abstract public class BaseMarkerResolution implements IQuickFix {
	protected String label;
	protected String description;
	protected ICompilationUnit cUnit;
	
	public BaseMarkerResolution(ICompilationUnit compilationUnit){
		this.cUnit = compilationUnit;
	}
	
	protected final void init(){
		description = getPreview();
	}

	@Override
	public final String getLabel() {
		return label;
	}
	
	protected void do_run(boolean leaveDirty){
		try{
			if(cUnit != null){
				ICompilationUnit compilationUnit = cUnit.getWorkingCopy(new NullProgressMonitor());
				
				TextFileChange change = getChange(compilationUnit);
				
				if(change.getEdit().hasChildren()){
					if(leaveDirty){
						change.setSaveMode(TextFileChange.LEAVE_DIRTY);
					}
					change.perform(new NullProgressMonitor());
					cUnit.reconcile(ICompilationUnit.NO_AST, false, null, new NullProgressMonitor());
				}
				compilationUnit.discardWorkingCopy();
			}
		}catch(CoreException ex){
			CommonPlugin.getDefault().logError(ex);
		}
	}

	@Override
	public final void run(IMarker marker) {
		do_run(false);
	}

	@Override
	public final String getDescription() {
		return description;
	}
	
	private TextChange getPreviewChange(){
		if(cUnit != null){
			try {
				ICompilationUnit compilationUnit = cUnit.getWorkingCopy(new NullProgressMonitor());
				
				TextChange change = getChange(compilationUnit);
				
				compilationUnit.discardWorkingCopy();
				return change;
			} catch (JavaModelException e) {
				CommonPlugin.getDefault().logError(e);
			}
			
		}
		return null;
	}
	
	private String getPreview(){
		TextChange previewChange = getPreviewChange();
		if(previewChange != null){
			try {
				return MarkerResolutionUtils.getPreview(previewChange);
			} catch (CoreException e) {
				CommonPlugin.getDefault().logError(e);
			}
		}
		return label;
	}
	
	@Override
	public void apply(IDocument document) {
		do_run(true);
	}

	@Override
	public Point getSelection(IDocument document) {
		return null;
	}

	@Override
	public String getAdditionalProposalInfo() {
		return description;
	}

	@Override
	public String getDisplayString() {
		return label;
	}

	@Override
	public IContextInformation getContextInformation() {
		return null;
	}

	@Override
	public int getRelevance() {
		return 100;
	}
	
	abstract protected TextFileChange getChange(ICompilationUnit compilationUnit);
}
