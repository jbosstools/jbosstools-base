/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.text.xml.contentassist;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.custom.BusyIndicator;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;
import org.osgi.framework.Bundle;

public class ContentAssistProcessorDefinition {
	private String fId = null;
	private String fClassName = null;

	// a list of content types (String) 
	private Map<String, List<String>> fContentTypes = null;

	private IConfigurationElement fConfigurationElement = null;

	/**
	 * @param id
	 * @param class1
	 * @param configurationElement
	 */
	public ContentAssistProcessorDefinition(String id, String class1, IConfigurationElement configurationElement) {
		super();
		fId = id;
		fClassName = class1;
		fConfigurationElement = configurationElement;
		fContentTypes = new HashMap<String, List<String>>();
	}

	public void addPartitionType(String contentType, String partitionType) {
		List<String> partitionTypes = new ArrayList<String>();
		
		if (fContentTypes.containsKey(contentType)) {
			partitionTypes = fContentTypes.get(contentType);
		}
		
		if (!partitionTypes.contains(partitionType)) {
			partitionTypes.add(partitionType);
			fContentTypes.put(contentType, partitionTypes);
		}
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
	 * @return Returns the fId.
	 */
	public String getId() {
		return fId;
	}

	/**
	 * @return IContentAssistProcessor for this definition
	 */
	public IContentAssistProcessor createContentAssistProcessor() {
		IContentAssistProcessor contentAssistProcessor = null;

		if (getClassName() != null) {
			contentAssistProcessor = (IContentAssistProcessor) createExtension(ContentAssistProcessorBuilder.ATT_CLASS);
		}

		return contentAssistProcessor;
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
					catch (CoreException e) {
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
		XmlEditorPlugin.getPluginLog().logError("Error in creating extension", x); //$NON-NLS-1$
		result[0] = null;
	}
	
	/**
	 * @return Returns the collection of ContentTypes.
	 */
	public Collection<String> getContentTypes() {
		return fContentTypes.keySet();
	}
	
	/**
	 * @return Returns the list of Partition Types for the specified ContentType.
	 */
	public List<String> getPartitionTypes(String contentType) {
		return (fContentTypes == null || !fContentTypes.containsKey(contentType) ?
				new ArrayList<String>(): fContentTypes.get(contentType));
	}
	
}
