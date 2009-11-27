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
package org.jboss.tools.common.el.core.refactoring;

import java.util.ArrayList;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RenameProcessor;
import org.eclipse.ltk.internal.core.refactoring.Messages;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.ElCoreMessages;
import org.jboss.tools.common.model.project.ProjectHome;
import org.jboss.tools.common.text.ITextSourceReference;
import org.jboss.tools.common.util.FileUtil;

/**
 * @author Daniel Azarov
 */
public abstract class ELRenameProcessor extends RenameProcessor {
	protected static final String JAVA_EXT = "java"; //$NON-NLS-1$
	protected static final String XML_EXT = "xml"; //$NON-NLS-1$
	protected static final String XHTML_EXT = "xhtml"; //$NON-NLS-1$
	protected static final String JSP_EXT = "jsp"; //$NON-NLS-1$
	protected static final String PROPERTIES_EXT = "properties"; //$NON-NLS-1$
	
	protected static final RefactoringParticipant[] EMPTY_REF_PARTICIPANT = new  RefactoringParticipant[0];	
	
	protected static final String SEAM_PROPERTIES_FILE = "seam.properties"; //$NON-NLS-1$
	
	protected RefactoringStatus status;
	
	protected CompositeChange rootChange;
	protected TextFileChange lastChange;
	protected IFile declarationFile=null;
	
	private ArrayList<String> keys = new ArrayList<String>();
	
	private String newName;
	private String oldName;
	
	private ELSearcher searcher = null;
	
	public ELRenameProcessor(IFile file, String oldName){
		searcher = new ELSearcher(file, oldName);
	}
	
	
	protected RefactorSearcher getSearcher(){
		return searcher;
	}
	
	public void setNewName(String newName){
		this.newName = newName;
	}
	
	protected String getNewName(){
		return newName;
	}
	
	protected void setOldName(String oldName){
		this.oldName = oldName;
	}
	
	public String getOldName(){
		return oldName;
	}
	
	// lets collect all changes for the same files in one MultiTextEdit
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
	
	private boolean isBadLocation(ITextSourceReference location, IFile file){
		boolean flag;
		if(location == null)
			flag = true;
		else
			flag = location.getStartPosition() == 0 && location.getLength() == 0;
		
//		if(flag)
//			status.addFatalError(Messages.format(ElCoreMessages.EL_RENAME_PROCESSOR_LOCATION_NOT_FOUND, file.getFullPath().toString()));
		return flag;
	}
	
	private void changeXMLNode(ITextSourceReference location, IFile file){
		if(isBadLocation(location, file))
			return;
		
		if(!isFileCorrect(file))
			return;

		String content = null;
		try {
			content = FileUtil.readStream(file);
		} catch (CoreException e) {
			ELCorePlugin.getDefault().logError(e);
		}

		String text = content.substring(location.getStartPosition(), location.getStartPosition()+location.getLength());
		if(text.startsWith("<")){ //$NON-NLS-1$
			int position = text.lastIndexOf("/>"); //$NON-NLS-1$
			if(position < 0){
				position = text.lastIndexOf(">"); //$NON-NLS-1$
			}
			change(file, location.getStartPosition()+position, 0, " name=\""+getNewName()+"\""); //$NON-NLS-1$ //$NON-NLS-2$
		}else{
			change(file, location.getStartPosition(), location.getLength(), getNewName());
		}
	}
	
	private void changeAnnotation(ITextSourceReference location, IFile file){
		if(isBadLocation(location, file))
			return;
		
		if(!isFileCorrect(file))
			return;

		String content = null;
		try {
			content = FileUtil.readStream(file);
		} catch (CoreException e) {
			ELCorePlugin.getDefault().logError(e);
		}

		String text = content.substring(location.getStartPosition(), location.getStartPosition()+location.getLength());
		int openBracket = text.indexOf("("); //$NON-NLS-1$
		int openQuote = text.indexOf("\""); //$NON-NLS-1$
		if(openBracket >= 0){
			int closeBracket = text.indexOf(")", openBracket); //$NON-NLS-1$
			
			int equals = text.indexOf("=", openBracket); //$NON-NLS-1$
			int value = text.indexOf("value", openBracket); //$NON-NLS-1$
			
			if(closeBracket == openBracket+1){ // empty brackets
				String newText = "\""+getNewName()+"\""; //$NON-NLS-1$ //$NON-NLS-2$
				change(file, location.getStartPosition()+openBracket+1, 0, newText);
			}else if(value > 0){ // construction value="name" found so change name
				String newText = text.replace(getOldName(), getNewName());
				change(file, location.getStartPosition(), location.getLength(), newText);
			}else if(equals > 0){ // other parameters are found
				String newText = "value=\""+getNewName()+"\","; //$NON-NLS-1$ //$NON-NLS-2$
				change(file, location.getStartPosition()+openBracket+1, 0, newText);
			}else{ // other cases
				String newText = text.replace(getOldName(), getNewName());
				change(file, location.getStartPosition(), location.getLength(), newText);
			}
		}else if(openQuote >= 0){
			int closeQuota = text.indexOf("\"", openQuote); //$NON-NLS-1$
			
			if(closeQuota == openQuote+1){ // empty quotas
				String newText = "\""+getNewName()+"\""; //$NON-NLS-1$ //$NON-NLS-2$
				change(file, location.getStartPosition()+openQuote+1, 0, newText);
			}else{ // the other cases
				String newText = text.replace(getOldName(), getNewName());
				change(file, location.getStartPosition(), location.getLength(), newText);
			}
		}else{
			String newText = "(\""+getNewName()+"\")"; //$NON-NLS-1$ //$NON-NLS-2$
			change(file, location.getStartPosition()+location.getLength(), 0, newText);
		}
	}
	
	private void clearChanges(){
		keys.clear();
	}
	
	private void change(IFile file, int offset, int length, String text){
		//System.out.println("change file - "+file.getFullPath()+" offset - "+offset+" len - "+length+" text"+text);
		String key = file.getFullPath().toString()+" "+offset;
		if(!keys.contains(key)){
			TextFileChange change = getChange(file);
			TextEdit edit = new ReplaceEdit(offset, length, text);
			change.addEdit(edit);
			keys.add(key);
		}
	}
	
	protected boolean isFileCorrect(IFile file){
		if(!file.isSynchronized(IResource.DEPTH_ZERO)){
			status.addFatalError(Messages.format(ElCoreMessages.EL_RENAME_PROCESSOR_OUT_OF_SYNC_FILE, file.getFullPath().toString()));
			return false;
		}else if(file.isPhantom()){
			status.addFatalError(Messages.format(ElCoreMessages.EL_RENAME_PROCESSOR_ERROR_PHANTOM_FILE, file.getFullPath().toString()));
			return false;
		}else if(file.isReadOnly()){
			status.addFatalError(Messages.format(ElCoreMessages.EL_RENAME_PROCESSOR_ERROR_READ_ONLY_FILE, file.getFullPath().toString()));
			return false;
		}
		return true;
	}


	public class ELSearcher extends RefactorSearcher{
		
		public ELSearcher(IFile file, String oldName){
			super(file, oldName);
		}
		
		
		private boolean checkFolder(IResource resource, IResource[] sources, IPath output){
			for(IResource folder : sources){
				if(resource.equals(folder))
					return false;
			}
			
			if(resource.getFullPath().equals(output))
				return false;
			
			return true;
		}
		
		ArrayList<String> keys = new ArrayList<String>();
		
		@Override
		protected IProject[] getProjects() {
			return new IProject[]{baseFile.getProject()};
		}
		
		@Override
		protected IContainer getViewFolder(IProject project) {
			IPath path = ProjectHome.getFirstWebContentPath(baseFile.getProject());
			
			if(path != null)
				return project.getFolder(path.removeFirstSegments(1));
			
			return null;
		}
		
		@Override
		protected void match(IFile file, int offset, int length) {
			change(file, offset, length, newName);
		}


		@Override
		protected boolean isFileCorrect(IFile file) {
			return ELRenameProcessor.this.isFileCorrect(file);
		}
	
	}
}