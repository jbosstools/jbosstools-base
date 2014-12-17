/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.validation.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.validation.IProjectValidationContext;
import org.jboss.tools.common.validation.ValidationResourceRegister;
import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

/**
 * Contains information for validators that must be saved between
 * validation invoking.
 * @author Alexey Kazakov
 */
public class ProjectValidationContext implements IProjectValidationContext {
	static String VALIDATION = "validation"; //$NON-NLS-1$
	static String CORE = "core"; //$NON-NLS-1$
	static String EL = "el"; //$NON-NLS-1$
	static String ALIASES = "aliases"; //$NON-NLS-1$
	static String ALIAS = "alias"; //$NON-NLS-1$
	static String PATH = "path"; //$NON-NLS-1$
	static String VALUE = "value"; //$NON-NLS-1$
	static String FULL_VALIDATION_REQUIRED = "fullValidationRequired"; //$NON-NLS-1$
	static String VALIDATOR_ID = "validator-id"; //$NON-NLS-1$
	static String TRUE = "true"; //$NON-NLS-1$

	// We should load/save these collections between eclipse sessions.
	private Map<String, LinkCollection> coreLinks = new HashMap<String, LinkCollection>();
	private ELValidatorContext elLinks = new ELValidatorContext("jboss.el"); //$NON-NLS-1$

	private Map<String, Set<String>> oldVariableNamesForELValidation = new HashMap<String, Set<String>>();

	private ValidationResourceRegister validationResourceRegister;

	private boolean fullValidationRequired = false;

	public ProjectValidationContext() {}

	public LinkCollection getCoreLinks(String validatorId) {
		LinkCollection linkCollection = coreLinks.get(validatorId);
		if(linkCollection==null) {
			linkCollection = new LinkCollection(validatorId);
			if(validatorId.equals("jboss.cdi.core")) {
				linkCollection.disableResourcesByVariableName();
			}
			coreLinks.put(validatorId, linkCollection);
		}
		return linkCollection;
	}

	private Set<String> getOldVariableNamesForELValidation(String validatorId) {
		Set<String> linkCollection = oldVariableNamesForELValidation.get(validatorId);
		if(linkCollection==null) {
			linkCollection = new HashSet<String>();
			oldVariableNamesForELValidation.put(validatorId, linkCollection);
		}
		return linkCollection;
	}

	private Set<String> getIds() {
		Set<String> ids = new HashSet<String>();
		ids.addAll(coreLinks.keySet());
		ids.addAll(oldVariableNamesForELValidation.keySet());
		return ids;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#addLinkedCoreResource(java.lang.String, org.eclipse.core.runtime.IPath, boolean)
	 */
	public void addLinkedCoreResource(String validatorId, String variableName, IPath linkedResourcePath, boolean declaration) {
		getCoreLinks(validatorId).addLinkedResource(variableName, linkedResourcePath, declaration);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeLinkedCoreResource(java.lang.String, org.eclipse.core.runtime.IPath)
	 */
	public void removeLinkedCoreResource(String validatorId, String name, IPath linkedResourcePath) {
		getCoreLinks(validatorId).removeLinkedResource(name, linkedResourcePath);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeLinkedCoreResources(java.util.Set)
	 */
	public void removeLinkedCoreResources(String validatorId, Set<IPath> resources) {
		getCoreLinks(validatorId).removeLinkedResources(resources);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeLinkedCoreResource(org.eclipse.core.runtime.IPath)
	 */
	public void removeLinkedCoreResource(String validatorId, IPath resource) {
		getCoreLinks(validatorId).removeLinkedResource(resource);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getCoreResourcesByVariableName(java.lang.String, boolean)
	 */
	public Set<IPath> getCoreResourcesByVariableName(String validatorId, String variableName, boolean declaration) {
		return getCoreLinks(validatorId).getResourcesByVariableName(variableName, declaration);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getVariableNamesByCoreResource(org.eclipse.core.runtime.IPath, boolean)
	 */
	public Set<String> getVariableNamesByCoreResource(String validatorId, IPath fullPath, boolean declaration) {
		return getCoreLinks(validatorId).getVariableNamesByResource(fullPath, declaration);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#addUnnamedCoreResource(org.eclipse.core.runtime.IPath)
	 */
	public void addUnnamedCoreResource(String validatorId, IPath fullPath) {
		getCoreLinks(validatorId).addUnnamedResource(fullPath);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getUnnamedCoreResources()
	 */
	public Set<IPath> getUnnamedCoreResources(String validatorId) {
		return getCoreLinks(validatorId).getUnnamedResources();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeUnnamedCoreResource(org.eclipse.core.runtime.IPath)
	 */
	public void removeUnnamedCoreResource(String validatorId, IPath fullPath) {
		getCoreLinks(validatorId).removeUnnamedResource(fullPath);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getUnnamedElResources()
	 */
	public Set<IPath> getUnnamedElResources() {
		return elLinks.getUnnamedResources();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeUnnamedElResource(org.eclipse.core.runtime.IPath)
	 */
	public void removeUnnamedElResource(IPath fullPath) {
		elLinks.removeUnnamedResource(fullPath);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#addVariableNameForELValidation(java.lang.String)
	 */
	public void addVariableNameForELValidation(String validatorId, String name) {
		getOldVariableNamesForELValidation(validatorId).add(name);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeLinkedEls(java.util.Set)
	 */
	public void removeLinkedEls(Set<IFile> resorces) {
		elLinks.removeLinkedEls(resorces);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getElsForValidation(java.util.Set, boolean)
	 */
	public Set<ELReference> getElsForValidation(Set<IFile> changedFiles, boolean onlyChangedVariables) {
		Set<ELReference> result = new HashSet<ELReference>();
		for (String id : getIds()) {
			Set<String> oldVariableNamesForELValidation = getOldVariableNamesForELValidation(id);
			// Collect all ELs which use new variables names
			for(IResource resource : changedFiles) {
				Set<String> newNames = getVariableNamesByCoreResource(id, resource.getFullPath(), true);
				if(newNames!=null) {
					for (String newName : newNames) {
						if(!onlyChangedVariables || (oldVariableNamesForELValidation!=null && !oldVariableNamesForELValidation.contains(newName))) {
							Set<ELReference> els = elLinks.getElsByVariableName(newName);
							if(els!=null) {
								result.addAll(els);
							}
						}
					}
				}
				// Threat resource path as a variable name too.
				Set<ELReference> els = elLinks.getElsByVariableName(resource.getFullPath().toString());
				if(els!=null) {
					result.addAll(els);
				}
				if(oldVariableNamesForELValidation!=null) {
					for (String oldName :oldVariableNamesForELValidation) {
						if(!onlyChangedVariables || newNames==null || !newNames.contains(oldName)) {
							els = elLinks.getElsByVariableName(oldName);
							if(els!=null) {
								result.addAll(els);
							}
						}
					}
				}
			}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#clearAll()
	 */
	public void clearAll() {
		for (LinkCollection links : coreLinks.values()) {
			links.clearAll();
		}
		coreLinks.clear();
		elLinks.clearAll();
		oldVariableNamesForELValidation.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#clearOldVariableNameForElValidation()
	 */
	public void clearOldVariableNameForElValidation() {
		oldVariableNamesForELValidation.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#addLinkedEl(java.lang.String, org.jboss.tools.jst.web.kb.validation.ELReference)
	 */
	public void addLinkedEl(String variableName, ELReference el) {
		elLinks.addLinkedEl(variableName, el);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#removeLinkedEl(java.lang.String, org.jboss.tools.jst.web.kb.validation.ELReference)
	 */
	public void removeLinkedEl(String name, ELReference el) {
		elLinks.removeLinkedEl(name, el);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IProjectValidationContext#removeLinkedEl(org.jboss.tools.common.el.core.ELReference)
	 */
	public void removeLinkedEl(ELReference el) {
		elLinks.removeLinkedEl(el);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#getElsByVariableName(java.lang.String)
	 */
	public Set<ELReference> getElsByVariableName(String variableName) {
		return elLinks.getElsByVariableName(variableName);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#store(org.w3c.dom.Element)
	 */
	public void store(Element root) {
		Map<String, String> pathAliases = new HashMap<String, String>();
		Element validation = XMLUtilities.createElement(root, VALIDATION);
		if(isFullValidationRequired()) {
			validation.setAttribute(FULL_VALIDATION_REQUIRED, TRUE);
		}
		for (LinkCollection links : coreLinks.values()) {
			Element core = XMLUtilities.createElement(validation, CORE);
			core.setAttribute(VALIDATOR_ID, links.getId());
			links.store(core, pathAliases);
		}
		Element el = XMLUtilities.createElement(validation, EL);
		elLinks.store(el, pathAliases);
		
		Element aliases = XMLUtilities.createElement(root, ALIASES);
		for (String path: pathAliases.keySet()) {
			String value = pathAliases.get(path);
			Element alias = XMLUtilities.createElement(aliases, ALIAS);
			alias.setAttribute(PATH, path);
			alias.setAttribute(VALUE, value);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationContext#load(org.w3c.dom.Element)
	 */
	public void load(Element root) {
		Map<String, String> pathAliases = new HashMap<String, String>();
		Element aliases = XMLUtilities.getUniqueChild(root, ALIASES);
		if(aliases != null) {
			Element[] aliasArray = XMLUtilities.getChildren(aliases, ALIAS);
			for (Element alias: aliasArray) {
				String path = alias.getAttribute(PATH);
				String value = alias.getAttribute(VALUE);
				pathAliases.put(value, path);
			}
		}

		Element validation = XMLUtilities.getUniqueChild(root, VALIDATION);
		if(validation == null) return;
		setFullValidationRequired(TRUE.equals(validation.getAttribute(FULL_VALIDATION_REQUIRED)));
		Element[] cores = XMLUtilities.getChildren(validation, CORE);
		for (Element core : cores) {
			String id = core.getAttribute(VALIDATOR_ID);
			if(id!=null && id.trim().length()>0) {
				getCoreLinks(id).load(core, pathAliases);
			}
		}
		Element[] els = XMLUtilities.getChildren(validation, EL);
		for (Element el : els) {
			elLinks.load(el, pathAliases);
		}
	}

	public int getModificationsSinceLastStore() {
		int result = 0;
		for (LinkCollection links : coreLinks.values()) {
			result = result + links.getModificationsSinceLastStore();
		}
		result = result + elLinks.getModificationsSinceLastStore();
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IProjectValidationContext#setValidationResourceRegister(org.jboss.tools.jst.web.kb.internal.validation.ValidationResourceRegister)
	 */
	public void setValidationResourceRegister(ValidationResourceRegister validationResourceRegister) {
		this.validationResourceRegister = validationResourceRegister;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IProjectValidationContext#getValidationResourceRegister()
	 */
	public ValidationResourceRegister getValidationResourceRegister() {
		return validationResourceRegister;
	}

	@Override
	public boolean isFullValidationRequired() {
		return fullValidationRequired;
	}

	@Override
	public void setFullValidationRequired(boolean b) {
		fullValidationRequired = b;
	}
}