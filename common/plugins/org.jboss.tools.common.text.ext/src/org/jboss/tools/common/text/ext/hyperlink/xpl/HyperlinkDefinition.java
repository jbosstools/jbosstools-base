/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.swt.custom.BusyIndicator;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkBuilder;
import org.osgi.framework.Bundle;

/**
 * Open on definition object
 * @author amywu
 */
public class HyperlinkDefinition {
	private String fId = null;
	private String fClassName = null;

	// a hash map of content type Ids (String) that points to lists of parition types (List of Strings)
	// contentTypeId -> List(paritionType, paritionType, partitionType, ...)
	// contentTypeId2 -> List(partitionType, partitionType, ...)
	// ...
	private HashMap fContentTypes = null;

	private IConfigurationElement fConfigurationElement = null;

	/**
	 * @param id
	 * @param class1
	 * @param configurationElement
	 */
	public HyperlinkDefinition(String id, String class1, IConfigurationElement configurationElement) {
		super();
		fId = id;
		fClassName = class1;
		fConfigurationElement = configurationElement;
		fContentTypes = new HashMap();
	}

	public void addContentTypeId(String contentTypeId) {
		if (!fContentTypes.containsKey(contentTypeId))
			fContentTypes.put(contentTypeId, new ArrayList());
	}

	public void addPartitionType(String contentTypeId, String partitionType) {
		if (!fContentTypes.containsKey(contentTypeId))
			fContentTypes.put(contentTypeId, new ArrayList());

		List partitionList = (List) fContentTypes.get(contentTypeId);
		partitionList.add(partitionType);
	}

	/**
	 * @return Returns the fClass.
	 */
	public String getClassName() {
		return fClassName;
	}

	/**
	 * @return Returns the fConfigurationElement.
	 */
	public IConfigurationElement getConfigurationElement() {
		return fConfigurationElement;
	}

	/**
	 * @return Returns the fContentTypes.
	 */
	public HashMap getContentTypes() {
		return fContentTypes;
	}

	/**
	 * @return Returns the fId.
	 */
	public String getId() {
		return fId;
	}

	/**
	 * @return IHyperlink for this definition
	 */
	public IHyperlink createHyperlink() {
		IHyperlink hyperlink = null;

		if (getClassName() != null) {
			hyperlink = (IHyperlink) createExtension(HyperlinkBuilder.ATT_CLASS);
		}

		return hyperlink;
	}

	/**
	 * Creates an extension.  If the extension plugin has not
	 * been loaded a busy cursor will be activated during the duration of
	 * the load.
	 * @param propertyName
	 * @return Object
	 */
	private Object createExtension(String propertyName) {
		// If plugin has been loaded create extension.
		// Otherwise, show busy cursor then create extension.
		final IConfigurationElement element = getConfigurationElement();
		final String name = propertyName;

		final Object[] result = new Object[1];
		Bundle bundle = Platform.getBundle(element.getDeclaringExtension().getNamespaceIdentifier()); 
		if (bundle.getState() == org.osgi.framework.Bundle.ACTIVE) {
			try {
				return element.createExecutableExtension(name);
			}
			catch (CoreException e) {
				handleCreateExecutableException(result, e);
			}
		}
		else {
			BusyIndicator.showWhile(null, new Runnable() {
				public void run() {
					try {
						result[0] = element.createExecutableExtension(name);
					}
					catch (Exception e) {
						handleCreateExecutableException(result, e);
					}
				}
			});
		}
		return result[0];
	}

	/**
	 * @param result
	 * @param e
	 */
	private void handleCreateExecutableException(Object[] result, Exception x) {
		ExtensionsPlugin.log("Error in creating extension", x);
		result[0] = null;
	}
}