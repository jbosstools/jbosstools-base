/*******************************************************************************
  * Copyright (c) 2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.el.ui.refactoring;

import java.util.ArrayList;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant;
import org.eclipse.ltk.internal.core.refactoring.Messages;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
import org.jboss.tools.common.el.core.refactoring.RefactorSearcher;
import org.jboss.tools.common.el.ui.ElUiCoreMessages;

public class RenameMethodParticipant extends RenameParticipant{
	private IMethod method;
	private String oldName;
	private String newName;
	private SeamRenameMethodSearcher searcher;
	private RefactoringStatus status;
	private CompositeChange rootChange;
	private TextFileChange lastChange;
	private ArrayList<String> keys = new ArrayList<String>();
	private boolean resolved = true;
	
	private static boolean added = false;
	
	
	@Override
	public RefactoringStatus checkConditions(IProgressMonitor pm,
			CheckConditionsContext context) throws OperationCanceledException {
		if(searcher == null)
			return status;
		
		if(method != null && !added){
			if(searcher.isGetter(method))
				status.addWarning(ElUiCoreMessages.RENAME_METHOD_PARTICIPANT_GETTER_WARNING);
			else if(searcher.isSetter(method))
				status.addWarning(ElUiCoreMessages.RENAME_METHOD_PARTICIPANT_SETTER_WARNING);
			added = true;
		}
		
		searcher.findELReferences();
		
		// TODO: find good phrase and externalize it
		if(!resolved)
			status.addWarning("Some changes were not resolved.");
		
		return status;
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		return rootChange;
	}

	@Override
	public String getName() {
		return oldName;
	}

	@Override
	protected boolean initialize(Object element) {
		if(element instanceof IMethod){
			status = new RefactoringStatus();
			
			rootChange = new CompositeChange("");
			method = (IMethod)element;
			
			oldName = SeamRenameMethodSearcher.getPropertyName(method.getElementName());
			
			newName = SeamRenameMethodSearcher.getPropertyName(getArguments().getNewName());
			searcher = new SeamRenameMethodSearcher((IFile)method.getResource(), oldName);
			added = false;
			return true;
		}
		return false;
	}
	
	protected TextFileChange getChange(IFile file){
		if(lastChange != null && lastChange.getFile().equals(file))
			return lastChange;
		
		for(int i=0; i < rootChange.getChildren().length; i++){
			TextFileChange change = (TextFileChange)rootChange.getChildren()[i];
			if(change.getFile().equals(file)){
				lastChange = change;
				return lastChange;
			}
		}
		lastChange = new TextFileChange(file.getName(), file);
		MultiTextEdit root = new MultiTextEdit();
		lastChange.setEdit(root);
		rootChange.add(lastChange);
		
		return lastChange;
	}
	
	private void change(IFile file, int offset, int length, String text){
		//System.out.println("change file - "+file.getFullPath()+" offset - "+offset+" len - "+length+" text <"+text+">");
		String key = file.getFullPath().toString()+" "+offset;
		if(!keys.contains(key)){
			TextFileChange change = getChange(file);
			TextEdit edit = new ReplaceEdit(offset, length, text);
			change.addEdit(edit);
			keys.add(key);
		}
	}
	
	class SeamRenameMethodSearcher extends RefactorSearcher{
		public SeamRenameMethodSearcher(IFile file, String name){
			super(file, name, method);
		}

		@Override
		protected boolean isFileCorrect(IFile file) {
			if(!file.isSynchronized(IResource.DEPTH_ZERO)){
				status.addFatalError(Messages.format(ElUiCoreMessages.RENAME_METHOD_PARTICIPANT_OUT_OF_SYNC_FILE, file.getFullPath().toString()));
				return false;
			}else if(file.isPhantom()){
				status.addFatalError(Messages.format(ElUiCoreMessages.RENAME_METHOD_PARTICIPANT_ERROR_PHANTOM_FILE, file.getFullPath().toString()));
				return false;
			}else if(file.isReadOnly()){
				status.addFatalError(Messages.format(ElUiCoreMessages.RENAME_METHOD_PARTICIPANT_ERROR_READ_ONLY_FILE, file.getFullPath().toString()));
				return false;
			}
			return true;
		}
		
		protected IProject[] getProjects(){
			IProject[] projects = new IProject[1];
			projects[0] = baseFile.getProject();
			return projects;
		}
		
		protected IContainer getViewFolder(IProject project){
			return null;
		}
		
		protected ELInvocationExpression findComponentReference(ELInvocationExpression invocationExpression){
			ELInvocationExpression invExp = invocationExpression;
			while(invExp != null){
				if(invExp instanceof ELMethodInvocation || invExp instanceof ELPropertyInvocation){
					if(invExp.getMemberName() != null && invExp.getMemberName().equals(propertyName))
						return invExp;
					else
						invExp = invExp.getLeft();
				}else{
					invExp = invExp.getLeft();
				}
			}
			return null;
		}

		@Override
		protected void match(IFile file, int offset, int length, boolean resolved) {
			change(file, offset, length, newName);
			if(!resolved)
				RenameMethodParticipant.this.resolved = false;
		}
	}

}
