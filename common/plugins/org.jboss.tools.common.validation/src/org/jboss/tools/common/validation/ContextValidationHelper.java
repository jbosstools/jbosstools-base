/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.wst.validation.internal.operations.WorkbenchContext;
import org.eclipse.wst.validation.internal.operations.WorkbenchReporter;
import org.jboss.tools.common.CommonPlugin;

/**
 * Helper for Validators that use Validator Context. 
 * @author Alexey Kazakov
 */
public class ContextValidationHelper extends WorkbenchContext {

	protected IValidationContextManager validationContextManager;
	protected TextFileDocumentProvider documentProvider = new TextFileDocumentProvider();

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.operations.WorkbenchContext#initialize()
	 */
	@Override
	public void initialize() {
		super.initialize();
		cleanup();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.operations.WorkbenchContext#deleting()
	 */
	@Override
	public void deleting() {
		super.deleting();
		cleanup();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.operations.WorkbenchContext#cleanup(org.eclipse.wst.validation.internal.operations.WorkbenchReporter)
	 */
	@Override
	public void cleanup(WorkbenchReporter reporter) {
		super.cleanup(reporter);
		cleanup();
	}

	public void cleanup() {
		if(validationContextManager!=null) {
			validationContextManager.setValidationResourceRegister(null);
		}
		validationContextManager = null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.operations.WorkbenchContext#registerResource(org.eclipse.core.resources.IResource)
	 */
	@Override
	public void registerResource(IResource resource) {
		if(resource instanceof IFile) {
			IFile file = (IFile)resource;
			if(validationContextManager == null) {
				validationContextManager = new ValidationContext(file.getProject());
			} else if(validationContextManager.isObsolete()) {
				validationContextManager.init(file.getProject()); // https://issues.jboss.org/browse/JBIDE-8726
			}
			validationContextManager.addProject(file.getProject());
			if(!file.exists()) {
				validationContextManager.addRemovedFile(file);
			} else {
				validationContextManager.registerFile(file);
			}
		}
	}

	/**
	 * @return Set of changed resources
	 */
	public Set<IFile> getChangedFiles() {
		Set<IFile> result = new HashSet<IFile>();
		String[] uris = getURIs();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		Set<IProject> projects = getAllProjects();
		for (int i = 0; i < uris.length; i++) {
			IFile currentFile = root.getFile(new Path(uris[i]));
			if(projects.contains(currentFile.getProject())) {
				result.add(currentFile);
			}
		}
		Set<IFile> removedFiles = getValidationContextManager().getRemovedFiles();
		for (IFile file : removedFiles) {
			if(projects.contains(file.getProject())) {
				result.add(file);
			}
		}
		return result;
	}

	public Set<IFile> getProjectSetRegisteredFiles() {
		Set<IFile> result = new HashSet<IFile>();
		Set<IFile> files = getValidationContextManager().getRegisteredFiles();
		Set<IProject> projects = getAllProjects();
		for (IFile file : files) {
			if(projects.contains(file.getProject())) {
				result.add(file);
			}
		}
		return result;
	}

	private Set<IProject> getAllProjects() {
		IProject project = getProject();
		if(!project.isAccessible()) {
			return Collections.emptySet();
		}

		List<IValidator> validators = getValidationContextManager().getValidators();
		Set<IProject> projects = new HashSet<IProject>();
		for (IValidator validator : validators) {
			IValidatingProjectTree tree = validator.getValidatingProjects(project);
			if(tree == null) {
				CommonPlugin.getDefault().logError(new IllegalStateException(MessageFormat.format(ValidationMessages.ERR_ILLIGAL_VALIDATION_STATE,validator,getProject(),getProject().exists()))); //$NON-NLS-1$
			} else {
				projects.addAll(tree.getAllProjects());
			}
		}
		return projects;
	}

	public IValidationContextManager getValidationContextManager() {
		return getValidationContextManager(true);
	}

	public IValidationContextManager getValidationContextManager(boolean initialize) {
		if(!initialize) {
			return validationContextManager;
		}
		if(validationContextManager==null) {
			validationContextManager = new ValidationContext(getProject());
		}
		return validationContextManager;
	}

	public void setValidationContextManager(IValidationContextManager context) {
		validationContextManager = context;
	}

	public TextFileDocumentProvider getDocumentProvider() {
		return documentProvider;
	}
}