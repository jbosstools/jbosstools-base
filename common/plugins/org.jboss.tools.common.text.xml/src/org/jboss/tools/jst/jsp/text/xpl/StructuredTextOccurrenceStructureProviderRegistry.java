/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.jst.jsp.text.xpl;

/**
 * @author Jeremy
 */


import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

/**
 * @since 3.0
 */
public class StructuredTextOccurrenceStructureProviderRegistry {
	
	private static final String EXTENSION_POINT= "occurrenceStructureProviders"; //$NON-NLS-1$
	
	/** The map of descriptors, indexed by their identifiers. */
	private Map fDescriptors;
	private String fPluginId; 
	/**
	 * Creates a new instance. 
	 */
	public StructuredTextOccurrenceStructureProviderRegistry(String editorPluginId) {
		this.fPluginId = editorPluginId;
	}
	
	/**
	 * Returns an array of <code>IJavaFoldingProviderDescriptor</code> describing
	 * all extension to the <code>foldingProviders</code> extension point.
	 * 
	 * @return the list of extensions to the
	 *         <code>quickDiffReferenceProvider</code> extension point.
	 */
	public StructuredTextOccurrenceStructureProviderDescriptor[] getOccurrenceProviderDescriptors() {
		synchronized (this) {
			ensureRegistered();
			return (StructuredTextOccurrenceStructureProviderDescriptor[]) fDescriptors.values().toArray(new StructuredTextOccurrenceStructureProviderDescriptor[fDescriptors.size()]);
		}
	}
	
	/**
	 * Returns the folding provider with identifier <code>id</code> or
	 * <code>null</code> if no such provider is registered.
	 * 
	 * @param id the identifier for which a provider is wanted
	 * @return the corresponding provider, or <code>null</code> if none can be
	 *         found
	 */
	public StructuredTextOccurrenceStructureProviderDescriptor getOccurrenceProviderDescriptor(String id) {
		synchronized (this) {
			ensureRegistered();
			return (StructuredTextOccurrenceStructureProviderDescriptor) fDescriptors.get(id);
		}
	}
	
	/**
	 * Instantiates and returns the provider that is currently configured in the
	 * preferences.
	 * 
	 * @return the current provider according to the preferences
	 */
	public IStructuredTextOccurrenceStructureProvider getCurrentOccurrenceProvider(String id) {
		StructuredTextOccurrenceStructureProviderDescriptor desc= getOccurrenceProviderDescriptor(id);
		if (desc != null) {
			try {
				return desc.createProvider();
			} catch (CoreException e) {
//				EditorPlugin.log(e);
			}
		}
		return null;
	}
	
	/**
	 * Ensures that the extensions are read and stored in
	 * <code>fDescriptors</code>.
	 */
	private void ensureRegistered() {
		if (fDescriptors == null)
			reloadExtensions();
	}

	/**
	 * Reads all extensions.
	 * <p>
	 * This method can be called more than once in
	 * order to reload from a changed extension registry.
	 * </p>
	 */
	public void reloadExtensions() {
		IExtensionRegistry registry= Platform.getExtensionRegistry();
		Map map= new HashMap();

		IConfigurationElement[] elements= registry.getConfigurationElementsFor(fPluginId, EXTENSION_POINT);
		for (int i= 0; i < elements.length; i++) {
			StructuredTextOccurrenceStructureProviderDescriptor desc= new StructuredTextOccurrenceStructureProviderDescriptor(elements[i]);
			map.put(desc.getEditorId(), desc);
		}
		
		synchronized(this) {
			fDescriptors= Collections.unmodifiableMap(map);
		}
	}

}
