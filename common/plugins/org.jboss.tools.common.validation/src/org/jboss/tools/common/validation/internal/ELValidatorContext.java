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
package org.jboss.tools.common.validation.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.validation.ValidationELReference;
import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

/**
 * @author Alexey Kazakov
 */
public class ELValidatorContext extends LinkCollection {

	private Map<String, Set<ELReference>> elsByVariableName = new HashMap<String, Set<ELReference>>();
	private Map<ELReference, Set<String>> variableNamesByEl = new HashMap<ELReference, Set<String>>();
	private Map<IPath, Set<ELReference>> elsByResource = new HashMap<IPath, Set<ELReference>>();

	public ELValidatorContext(String id) {
		super(id);
	}

	/**
	 * Save link between EL and variable name.
	 * @param variableName
	 * @param el
	 */
	public synchronized void addLinkedEl(String variableName, ELReference el) {
		Set<ELReference> linkedEls = elsByVariableName.get(variableName);
		if(linkedEls==null) {
			// create set of linked ELs with variable name.
			linkedEls = new HashSet<ELReference>();
			elsByVariableName.put(variableName, linkedEls);
		}
		// save linked ELs.
		// don't save links if there are more than 500 ELs for the var name.
		if(linkedEls.size()<500) {
			if(linkedEls.add(el)) {
				modifications++;
			}
			// Save link between EL and variable names.
			Set<String> variableNames = variableNamesByEl.get(el);
			if(variableNames==null) {
				variableNames = new HashSet<String>();
				variableNamesByEl.put(el, variableNames);
			}
			if(variableNames.add(variableName)) {
				modifications++;
			}
		}

		// Save link between EL and resource.
		Set<ELReference> els = elsByResource.get(el.getPath());
		if(els==null) {
			els = new HashSet<ELReference>();
			elsByResource.put(el.getPath(), els);
		}
		if(els.add(el)) {
			modifications++;
		}
	}

	public synchronized void removeLinkedEls(Set<IFile> resorces) {
		for (IFile file : resorces) {
			removeLinkedEls(file);
		}
	}

	public synchronized void removeLinkedEls(IFile resource) {
		Set<ELReference> els = elsByResource.get(resource.getFullPath());
		if(els!=null) {
			if(elsByResource.remove(resource.getFullPath()) != null) {
				modifications++;
			}
			for (ELReference el : els) {
				Set<String> names = variableNamesByEl.get(el);
				if(names!=null) {
					String[] nameStrins = names.toArray(new String[0]);
					for (int i = 0; i < nameStrins.length; i++) {
						removeLinkedEl(nameStrins[i], el);
					}
				}
			}
		}
	}

	/**
	 * Removes link between EL and variable name.
	 * @param name
	 * @param el
	 */
	public synchronized void removeLinkedEl(String name, ELReference el) {
		Set<ELReference> linkedEls = elsByVariableName.get(name);
		if(linkedEls!=null) {
			if(linkedEls.remove(el)) {
				modifications++;
			}
		}
		if(linkedEls.isEmpty()) {
			elsByVariableName.remove(name);
		}

		// Remove link between EL and variable names.
		Set<String> variableNames = variableNamesByEl.get(el);
		if(variableNames!=null) {
			if(variableNames.remove(name)) {
				modifications++;
			}
		}
		if(variableNames.isEmpty()) {
			variableNamesByEl.remove(el);
		}
	}

	/**
	 * Return ELs with given variable name
	 * @param variableName
	 * @return
	 */
	public synchronized Set<ELReference> getElsByVariableName(String variableName) {
		return elsByVariableName.get(variableName);
	}

	/**
	 * @param names
	 * @return
	 */
	public synchronized Set<ELReference> getElsByVariableNames(Set<String> names) {
		Set<ELReference> result = new HashSet<ELReference>();
		for(String name : names) {
			Set<ELReference> els = getElsByVariableName(name);
			if(els!=null) {
				result.addAll(els);
			}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.internal.validation.LinkCollection#clearAll()
	 */
	@Override
	public synchronized void clearAll() {
		super.clearAll();
		elsByVariableName.clear();
		variableNamesByEl.clear();
		elsByResource.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.internal.validation.LinkCollection#store(org.w3c.dom.Element)
	 */
	@Override
	public synchronized void store(Element root) {
		super.store(root);
		Set<String> variables = elsByVariableName.keySet();
		for (String name: variables) {
			Set<ELReference> els = elsByVariableName.get(name);
			if(els == null) {
				continue;
			}
			for (ELReference el: els) {
				Element linkedEl = XMLUtilities.createElement(root, "linked-el"); //$NON-NLS-1$
				linkedEl.setAttribute("name", name); //$NON-NLS-1$
				el.store(linkedEl);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.internal.validation.LinkCollection#load(org.w3c.dom.Element)
	 */
	@Override
	public synchronized void load(Element root) {
		super.load(root);
		if(root == null) {
			return;
		}
		Element[] linkedEls = XMLUtilities.getChildren(root, "linked-el"); //$NON-NLS-1$
		if(linkedEls != null) { 
			for (int i = 0; i < linkedEls.length; i++) {
				String name = linkedEls[i].getAttribute("name"); //$NON-NLS-1$
				if(name == null || name.trim().length() == 0) {
					continue;
				}
				ELReference el = new ValidationELReference();
				el.load(linkedEls[i]);
				el.setNeedToInitMarkers(true);
				addLinkedEl(name, el);
			}
		}
	}
}