/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.CommonPlugin;

/**
 * @author Alexey Kazakov
 */
public class ValidationContext implements IValidationContextManager {

	private ValidationResourceRegister validationResourceRegister;
	private Map<IValidator, IValidatingProjectTree> projectTree = new HashMap<IValidator, IValidatingProjectTree>();
	private static List<IConfigurationElement> ALL_VALIDATORS;
	private List<IValidator> validators = new ArrayList<IValidator>();
	private Map<IValidator, Set<IProject>> validatedProjects = new HashMap<IValidator, Set<IProject>>();

	public ValidationContext(IProject project) {
		init(project);
	}

	public void init(IProject project) {
		projectTree.clear();
		validators.clear();
		validationResourceRegister = null;
		if(ALL_VALIDATORS == null) {
			// Load all the validators
			ALL_VALIDATORS = new ArrayList<IConfigurationElement>();
	        IExtensionRegistry registry = Platform.getExtensionRegistry();
			IExtensionPoint extensionPoint = registry.getExtensionPoint(IValidator.EXTENSION_POINT_ID);
			if (extensionPoint != null) { 
				IExtension[] extensions = extensionPoint.getExtensions();
				for (int i=0; i<extensions.length; i++) {
					IExtension extension = extensions[i];
					IConfigurationElement[] elements = extension.getConfigurationElements();
					for(int j=0; j<elements.length; j++) {
						ALL_VALIDATORS.add(elements[j]);
					}
				}
			}
		}

		List<IValidator> dependentValidators = new ArrayList<IValidator>();
		List<IValidator> allValidators = new ArrayList<IValidator>();
		for (IConfigurationElement element : ALL_VALIDATORS) {
			try {
				IValidator validator = (IValidator)element.createExecutableExtension("class"); //$NON-NLS-1$
				String dependent = element.getAttribute("dependent"); //$NON-NLS-1$
				if(Boolean.parseBoolean(dependent)) {
					dependentValidators.add(validator);
				} else {
					allValidators.add(validator);
				}
			} catch (CoreException e) {
				CommonPlugin.getDefault().logError(e);
			}
		}
		// We should add all the dependent validators (e.g. EL validator) to the very end of the list.
		allValidators.addAll(dependentValidators);

		// Init context for given project.
		for (IValidator validator : allValidators) {
			if(validator.shouldValidate(project)) {
				IValidatingProjectTree prTree = validator.getValidatingProjects(project);
				if(prTree!=null) {
					validators.add(validator);
					projectTree.put(validator, prTree);
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#isObsolete()
	 */
	public boolean isObsolete() {
		return validationResourceRegister!=null && validationResourceRegister.isObsolete();
	}

	private ValidationResourceRegister getValidationResourceRegister() {
		if(validationResourceRegister==null && !projectTree.isEmpty()) {
			// Initialize the register
			for (IValidatingProjectTree tree : projectTree.values()) {
				boolean inited = false;
				if(!tree.getBrunches().isEmpty()) {
					for (IValidatingProjectSet brunch : tree.getBrunches().values()) {
						IProjectValidationContext context = brunch.getRootContext();
						ValidationResourceRegister register = context.getValidationResourceRegister();
						if(register==null) {
							if(validationResourceRegister==null) {
								validationResourceRegister = new ValidationResourceRegister();
							}
							context.setValidationResourceRegister(validationResourceRegister);
						} else {
							validationResourceRegister = register;
							inited = true;
							break;
						}
					}
				}
				if(inited) {
					break;
				}
			}
		}
		if(validationResourceRegister==null) {
			validationResourceRegister = new ValidationResourceRegister();
		}
		return validationResourceRegister;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#setValidationResourceRegister(org.jboss.tools.jst.web.kb.internal.validation.ValidationResourceRegister)
	 */
	public void setValidationResourceRegister(
			ValidationResourceRegister validationResourceRegister) {
		this.validationResourceRegister = validationResourceRegister;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getValidators()
	 */
	public List<IValidator> getValidators() {
		return validators;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.ISeamValidationContext#clearAll()
	 */
	public void clearAll() {
		clearRegisteredFiles();
		for (IValidatingProjectTree tree : projectTree.values()) {
			for (IValidatingProjectSet brunch : tree.getBrunches().values()) {
				brunch.getRootContext().clearAll();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#clearRegisteredFiles()
	 */
	public void clearRegisteredFiles() {
		getValidationResourceRegister().clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getRemovedFiles()
	 */
	public Set<IFile> getRemovedFiles() {
		return getValidationResourceRegister().getRemovedFiles();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#addRemovedFile(org.eclipse.core.resources.IFile)
	 */
	public void addRemovedFile(IFile file) {
		getValidationResourceRegister().addRemovedFile(file);
		for (IValidatingProjectTree tree : projectTree.values()) {
			tree.addProject(file.getProject());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getRegisteredFiles()
	 */
	public Set<IFile> getRegisteredFiles() {
		return getValidationResourceRegister().getRegisteredFiles();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#registerFile(org.eclipse.core.resources.IFile)
	 */
	public void registerFile(IFile file) {
		getValidationResourceRegister().registerFile(file);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#getValidatingProjectTree(org.jboss.tools.jst.web.kb.validation.IValidator)
	 */
	public IValidatingProjectTree getValidatingProjectTree(IValidator validator) {
		return projectTree.get(validator);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#addProject(org.eclipse.core.resources.IProject)
	 */
	public void addProject(IProject project) {
		for (IValidator validator : validators) {
			projectTree.get(validator).addProject(project);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#getRootProjects()
	 */
	public Set<IProject> getRootProjects() {
		Set<IProject> roots = new HashSet<IProject>();
		for (IValidatingProjectTree tree : projectTree.values()) {
			for (IValidatingProjectSet brunch : tree.getBrunches().values()) {
				roots.add(brunch.getRootProject());
			}
		}
		return roots;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#clearAllResourceLinks()
	 */
	public void clearAllResourceLinks() {
		for (IValidatingProjectTree tree : projectTree.values()) {
			for (IValidatingProjectSet brunch : tree.getBrunches().values()) {
				brunch.getRootContext().clearAll();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#addValidatedProject(org.jboss.tools.jst.web.kb.validation.IValidator, org.eclipse.core.resources.IProject)
	 */
	public void addValidatedProject(IValidator validator, IProject project) {
		Set<IProject> projects = validatedProjects.get(validator);
		if(projects==null) {
			projects = new HashSet<IProject>();
			validatedProjects.put(validator, projects);
		}
		projects.add(project);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#clearValidatedProjectsList()
	 */
	public void clearValidatedProjectsList() {
		validatedProjects.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContextManager#projectHasBeenValidated(org.jboss.tools.jst.web.kb.validation.IValidator, org.eclipse.core.resources.IProject)
	 */
	public boolean projectHasBeenValidated(IValidator validator, IProject project) {
		Set<IProject> projects = validatedProjects.get(validator);
		return projects==null?false:projects.contains(project);
	}
}