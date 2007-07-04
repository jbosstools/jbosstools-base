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

package org.jboss.tools.common.text.ext.contentassist;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.custom.BusyIndicator;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.osgi.framework.Bundle;

public class ContentAssistProcessorDefinition {
	private String fId = null;
	private String fClassName = null;

	// a list of partition types (String) 
	private List fPartitionTypes = null;

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
		fPartitionTypes = new ArrayList();
	}

	public void addPartitionType(String partitionType) {
		if (!fPartitionTypes.contains(partitionType))
			fPartitionTypes.add(partitionType);
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
		ExtensionsPlugin.getPluginLog().logError("Error in creating extension", x);
		result[0] = null;
	}
	
	/**
	 * @return Returns the fPartitionTypes.
	 */
	public List getPartitionTypes() {
		return fPartitionTypes;
	}
}
