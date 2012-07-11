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
import org.eclipse.ltk.core.refactoring.TextChange;
import org.eclipse.ui.IMarkerResolution2;
import org.jboss.tools.common.CommonPlugin;

abstract public class BaseMarkerResolution implements IMarkerResolution2 {
	protected String label;
	protected String description;
	
	protected final void init(){
		description = getPreview();
	}

	@Override
	public final String getLabel() {
		return label;
	}

	@Override
	public final void run(IMarker marker) {
		try{
			ICompilationUnit original = getCompilationUnit();
			if(original != null){
				ICompilationUnit compilationUnit = original.getWorkingCopy(new NullProgressMonitor());
				
				TextChange change = getChange(compilationUnit);
				
				if(change.getEdit().hasChildren()){
					change.perform(new NullProgressMonitor());
					original.reconcile(ICompilationUnit.NO_AST, false, null, new NullProgressMonitor());
				}
				compilationUnit.discardWorkingCopy();
			}
		}catch(CoreException ex){
			CommonPlugin.getDefault().logError(ex);
		}
	}

	@Override
	public final String getDescription() {
		return description;
	}
	
	private TextChange getPreviewChange(){
		ICompilationUnit original = getCompilationUnit();
		if(original != null){
			try {
				ICompilationUnit compilationUnit = original.getWorkingCopy(new NullProgressMonitor());
				
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
	
	abstract protected ICompilationUnit getCompilationUnit();
	
	abstract protected TextChange getChange(ICompilationUnit compilationUnit);

}
