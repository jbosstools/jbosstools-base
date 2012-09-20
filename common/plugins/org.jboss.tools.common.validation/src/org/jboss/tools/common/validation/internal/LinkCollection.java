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

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.util.UniquePaths;
import org.jboss.tools.common.validation.ValidationMessages;
import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

/**
 * @author Alexey Kazakov
 */
public class LinkCollection {
	protected Map<String, Set<IPath>> resourcesByVariableName = new HashMap<String, Set<IPath>>();
	protected Map<IPath, Set<String>> variableNamesByResource = new HashMap<IPath, Set<String>>();
	protected Map<String, Set<IPath>> resourcesByDeclaringVariableName = new HashMap<String, Set<IPath>>();
	protected Map<IPath, Set<String>> declaringVariableNamesByResource = new HashMap<IPath, Set<String>>();
	protected Set<IPath> unnamedResources = new HashSet<IPath>();
	private String id;

	public LinkCollection(String id) {
		this.id = id;
	}

	public void disableResourcesByVariableName() {
		resourcesByVariableName = null;
	}

	protected int modifications = 0;

	/**
	 * Save link between resource and variable name.
	 * It's needed for incremental validation because we must save all linked resources of changed java file.
	 */
	public void addLinkedResource(String variableName, IPath linkedResourcePath, boolean declaration) {
		if(linkedResourcePath==null) {
			throw new IllegalArgumentException(ValidationMessages.VALIDATION_CONTEXT_LINKED_RESOURCE_PATH_MUST_NOT_BE_NULL);
		}
		if(variableName==null) {
			throw new IllegalArgumentException(ValidationMessages.VALIDATION_CONTEXT_VARIABLE_NAME_MUST_NOT_BE_NULL);
		}
		
		linkedResourcePath = UniquePaths.getInstance().intern(linkedResourcePath);

		if(resourcesByVariableName != null) {
			synchronized(this) {
				Set<IPath> linkedResources = resourcesByVariableName.get(variableName);
				if(linkedResources==null) {
					// create set of linked resources with variable name.
					linkedResources = new HashSet<IPath>();
					resourcesByVariableName.put(variableName, linkedResources);
				}
				//save linked resources.
				if(linkedResources.add(linkedResourcePath)) {
					modifications++;
				}
			}
		}

		Set<String> variableNames = null;
		// Save link between resource and variable names. It's needed if variable name changes in resource file.
		synchronized(this) {
			variableNames = variableNamesByResource.get(linkedResourcePath);
			if(variableNames==null) {
				variableNames = new HashSet<String>();
				variableNamesByResource.put(linkedResourcePath, variableNames);
			}
			if(variableNames.add(variableName.intern())) {
				modifications++;
			}
		}

		if(declaration) {
			synchronized(this) {
				Set<IPath> linkedResources = resourcesByDeclaringVariableName.get(variableName);
				if(linkedResources==null) {
					// create set of linked resources with declaring variable name.
					linkedResources = new HashSet<IPath>();
					resourcesByDeclaringVariableName.put(variableName, linkedResources);
				}
				// save linked resources.
				if(linkedResources.add(linkedResourcePath)) {
					modifications++;
				}
			}

			// Save link between resource and declaring  variable names. It's needed if variable name changes in resource file.
			variableNames = declaringVariableNamesByResource.get(linkedResourcePath);
			if(variableNames==null) {
				variableNames = new HashSet<String>();
				declaringVariableNamesByResource.put(linkedResourcePath, variableNames);
			}
			if(variableNames.add(variableName)) {
				modifications++;
			}
		}
	}

	/**
	 * Removes link between resource and variable name.
	 * @param oldVariableName
	 * @param linkedResourcePath
	 */
	public void removeLinkedResource(String name, IPath linkedResourcePath) {
		if(resourcesByVariableName != null) {
			synchronized(this) {
				Set<IPath> linkedResources = resourcesByVariableName.get(name);
				if(linkedResources!=null) {
					// remove linked resource.
					if(linkedResources.remove(linkedResourcePath)) {
						modifications++;
					}
				}
				if(linkedResources.isEmpty()) {
					resourcesByVariableName.remove(name);
				}
			}
		}
		// Remove link between resource and declaring variable names.
		Set<String> variableNames = variableNamesByResource.get(linkedResourcePath);
		if(variableNames!=null) {
			if(variableNames.remove(name)) {
				modifications++;
			}
		}
		if(variableNames.isEmpty()) {
			variableNamesByResource.remove(linkedResourcePath);
		}
		synchronized(this) {
			Set<IPath> linkedResources = resourcesByDeclaringVariableName.get(name);
			if(linkedResources!=null) {
				// remove linked resource.
				if(linkedResources.remove(linkedResourcePath)) {
					modifications++;
				}
			}
			if(linkedResources.isEmpty()) {
				resourcesByDeclaringVariableName.remove(name);
			}
		}
		// Remove link between resource and declaring variable names.
		variableNames = declaringVariableNamesByResource.get(linkedResourcePath);
		if(variableNames!=null) {
			if(variableNames.remove(name)) {
				modifications++;
			}
		}
		if(variableNames.isEmpty()) {
			declaringVariableNamesByResource.remove(linkedResourcePath);
		}
	}

	/**
	 * Removes link between resources and variable names.
	 * @param linkedResources
	 */
	public void removeLinkedResources(Set<IPath> resources) {
		for (IPath resource : resources) {
			removeLinkedResource(resource);
		}
	}

	/**
	 * Removes link between resource and variable names.
	 * @param linkedResources
	 */
	public synchronized void removeLinkedResource(IPath resource) {
		Set<String> resourceNames = variableNamesByResource.get(resource);
		if(resourceNames!=null && resourcesByVariableName != null) {
			for (String name : resourceNames) {
				Set<IPath> linkedResources = resourcesByVariableName.get(name);
				if(linkedResources!=null) {
					if(linkedResources.remove(resource)) {
						modifications++;
					}
					if(linkedResources.isEmpty()) {
						resourcesByVariableName.remove(name);
					}
				}
			}
		}
		if(variableNamesByResource.remove(resource) != null) {
			modifications++;
		}

		resourceNames = declaringVariableNamesByResource.get(resource);
		if(resourceNames!=null) {
			for (String name : resourceNames) {
				Set<IPath> linkedResources = resourcesByDeclaringVariableName.get(name);
				if(linkedResources!=null) {
					if(linkedResources.remove(resource)) {
						modifications++;
					}
					if(linkedResources.isEmpty()) {
						resourcesByDeclaringVariableName.remove(name);
					}
				}
			}
		}
		if(declaringVariableNamesByResource.remove(resource) != null) {
			modifications++;
		}
	}

	public Set<IPath> getResourcesByVariableName(String variableName, boolean declaration) {
		if(!declaration && resourcesByVariableName == null) {
			throw new RuntimeException("ResourcesByVariableName are disabled.");
		}
		return declaration ? resourcesByDeclaringVariableName.get(variableName) : resourcesByVariableName.get(variableName);
	}

	public synchronized Set<String> getVariableNamesByResource(IPath fullPath, boolean declaration) {
		return declaration?declaringVariableNamesByResource.get(fullPath):variableNamesByResource.get(fullPath);
	}

	/**
	 * Adds resource without any link to any context variable name.
	 * @param fullPath
	 */
	public void addUnnamedResource(IPath fullPath) {
		if(unnamedResources.add(fullPath)) {
			modifications++;
		}
	}

	/**
	 * @return Set of resources without any link to any context variable name.
	 * @param fullPath
	 */
	public Set<IPath> getUnnamedResources() {
		return unnamedResources;
	}

	/**
	 * Removes unnamed resource.
	 * @param fullPath
	 */
	public void removeUnnamedResource(IPath fullPath) {
		if(unnamedResources.remove(fullPath)) {
			modifications++;
		}
	}

	/**
	 * Clear all references
	 */
	public synchronized void clearAll() {
		if(resourcesByVariableName != null) {
			resourcesByVariableName.clear();
		}
		variableNamesByResource.clear();
		declaringVariableNamesByResource.clear();
		resourcesByDeclaringVariableName.clear();
		unnamedResources.clear();
		modifications = 0;
	}

	/**
	 * Store the collection to XML
	 * @param root
	 */
	public synchronized void store(Element root, Map<String, String> pathAliases) {
		Set<IPath> paths = variableNamesByResource.keySet();
		for (IPath path: paths) {
			String pathAlias = ELReference.getAlias(pathAliases, path.toString());
			Set<String> variables = variableNamesByResource.get(path);
			if(variables == null || variables.isEmpty()) continue;
			StringBuilder declarationFalseNames = new StringBuilder();
			StringBuilder declarationTrueNames = new StringBuilder();
			for (String name: variables) {
				String nameAlias = ELReference.getAlias(pathAliases, name);
				if(checkDeclaration(path, name)) {
					declarationTrueNames.append(nameAlias).append(";");
				} else {
					declarationFalseNames.append(nameAlias).append(";");
				}
			}
			if(declarationFalseNames.length() > 0) {
				Element linkedResource = XMLUtilities.createElement(root, "linked-resource"); //$NON-NLS-1$
				linkedResource.setAttribute("path", pathAlias); //$NON-NLS-1$
				linkedResource.setAttribute("name", declarationFalseNames.toString()); //$NON-NLS-1$
			}
			if(declarationTrueNames.length() > 0) {
				Element linkedResource = XMLUtilities.createElement(root, "linked-resource"); //$NON-NLS-1$
				linkedResource.setAttribute("path", pathAlias); //$NON-NLS-1$
				linkedResource.setAttribute("name", declarationTrueNames.toString()); //$NON-NLS-1$
				linkedResource.setAttribute("declaration", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		StringBuilder unnamedPaths = new StringBuilder();
		for (IPath unnamedPath: unnamedResources) {
			String pathAlias = ELReference.getAlias(pathAliases, unnamedPath.toString());
			unnamedPaths.append(pathAlias).append(";");
		}
		if(unnamedPaths.length() > 0) {
			Element unnamedPathElement = XMLUtilities.createElement(root, "unnamed-path"); //$NON-NLS-1$
			unnamedPathElement.setAttribute("path", ELReference.getAlias(pathAliases, unnamedPaths.toString())); //$NON-NLS-1$
		}
		modifications = 0;
	}

	/**
	 * Load the collection from XML
	 * @param root
	 */
	public void load(Element root, Map<String, String> pathAliases) {
		if(root == null) return;
		Element[] linkedResources = XMLUtilities.getChildren(root, "linked-resource"); //$NON-NLS-1$
		if(linkedResources != null) for (int i = 0; i < linkedResources.length; i++) {
			String path = linkedResources[i].getAttribute("path"); //$NON-NLS-1$
			if(path == null || path.trim().length() == 0) continue;
			if(path.indexOf(';') > 0) {
				//support to old format
				path = path.substring(0, path.indexOf(';'));
			}
			path = ELReference.getPath(pathAliases, path);
			IPath pathObject = new Path(path);
			String name1 = linkedResources[i].getAttribute("name"); //$NON-NLS-1$
			if(name1 == null || name1.trim().length() == 0) continue;
			String declaration = linkedResources[i].getAttribute("declaration"); //$NON-NLS-1$
			boolean declarationFlag = "true".equals(declaration); //$NON-NLS-1$
			String[] names = name1.split(";");
			for (String name: names) {
				name = ELReference.getPath(pathAliases, name);
				addLinkedResource(name, pathObject, declarationFlag);
			}
		}
		Element[] unnamedPathElement = XMLUtilities.getChildren(root, "unnamed-path"); //$NON-NLS-1$
		if(unnamedPathElement != null) for (int i = 0; i < unnamedPathElement.length; i++) {
			String path1 = unnamedPathElement[i].getAttribute("path"); //$NON-NLS-1$
			String[] paths = path1.split(";");
			for (String path: paths) {
				path = ELReference.getPath(pathAliases, path);
				IPath pathObject = new Path(path);
				addUnnamedResource(pathObject);
			}
		}
		modifications = 0;
	}

	private boolean checkDeclaration(IPath resource, String variableName) {
		Set<IPath> paths = resourcesByDeclaringVariableName.get(variableName);
		if(paths!=null) {
			for (IPath path : paths) {
				if(path.equals(resource)) {
					return true;
				}
			}
		}
		return false;
	}

	public int getModificationsSinceLastStore() {
		return modifications;
	}

	public String getId() {
		return id;
	}

	public boolean isEmpty() {
		return (resourcesByVariableName == null || resourcesByVariableName.isEmpty()) && variableNamesByResource.isEmpty() && resourcesByDeclaringVariableName.isEmpty() && declaringVariableNamesByResource.isEmpty() && unnamedResources.isEmpty();
	}
}