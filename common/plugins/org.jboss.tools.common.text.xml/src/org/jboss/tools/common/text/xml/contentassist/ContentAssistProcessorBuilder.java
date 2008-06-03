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
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.wst.sse.ui.internal.util.Sorter;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;

public class ContentAssistProcessorBuilder extends org.eclipse.wst.sse.ui.internal.extension.RegistryReader {
	// extension point ID
	public static final String PL_CONTENTASSISTPROCESSOR = "contentAssistProcessor"; //$NON-NLS-1$

	public static final String TAG_CONTENTASSISTPROCESSOR = "contentAssistProcessor"; //$NON-NLS-1$
	public static final String TAG_PARTITION_TYPE = "partitiontype"; //$NON-NLS-1$

	public static final String ATT_ID = "id"; //$NON-NLS-1$
	public static final String ATT_CLASS = "class"; //$NON-NLS-1$

	protected String targetContributionTag;

	private static ContentAssistProcessorBuilder fInstance;

	private List<ContentAssistProcessorDefinition> fContentAssistProcessorDefs = null;
	private ContentAssistProcessorDefinition fCurrentDefinition = null;

	/**
	 * returns singleton instance of ContentAssistProcessorBuilder
	 * 
	 * @return ContentAssistProcessorBuilder
	 */
	public synchronized static ContentAssistProcessorBuilder getInstance() {
		if (fInstance == null) {
			fInstance = new ContentAssistProcessorBuilder();
		}
		return fInstance;
	}

	/**
	 * Returns the name of the part ID attribute that is expected
	 * in the target extension.
	 * 
	 * @param element
	 * @return String
	 */
	protected String getId(IConfigurationElement element) {
		String value = element.getAttribute(ATT_ID);
		return value;
	}

	protected String getContentAssistProcessorClass(IConfigurationElement element) {
		String value = element.getAttribute(ATT_CLASS);
		return value;
	}

	/**
	 * Processes element which should be a configuration element specifying an
	 * open on object.  Creates a new ContentAssistProcessor definitio object and adds it to the
	 * list of ContentAssistProcessor definition objects
	 * 
	 * @param element ContentAssistProcessor configuration element
	 */
	private void processContentAssistProcessorTag(IConfigurationElement element) {
		String theId = getId(element);
		String theClass = getContentAssistProcessorClass(element);

		if (theId != null && theClass != null) {
			// start building new HyperlinkDefinition
			fCurrentDefinition = new ContentAssistProcessorDefinition(theId, theClass, element);

			// create a new list of open on definitions if it hasn't been created yet
			if (fContentAssistProcessorDefs == null) {
				fContentAssistProcessorDefs = new ArrayList<ContentAssistProcessorDefinition>();
			}
			fContentAssistProcessorDefs.add(fCurrentDefinition);
		}
		else {
			fCurrentDefinition = null;
		}
	}

	/**
	 * Processes element which should be a configuration element specifying a partition
	 * type for the current contentAssistProcessor tag.  Assumes that there is a valid
	 * current contentAssistProcessor tag.
	 * 
	 * @param element partitiontype configuration element
	 */
	private void processPartitionTypeTag(IConfigurationElement element) {
		// add to current HyperlinkDefinition/contentType
		String theId = getId(element);

		if (theId != null) {
			fCurrentDefinition.addPartitionType(theId);
		}
	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.internal.extension.RegistryReader#readElement(org.eclipse.core.runtime.IConfigurationElement)
	 */
	protected boolean readElement(IConfigurationElement element) {
		String tag = element.getName();

		if (tag.equals(targetContributionTag)) {
			processContentAssistProcessorTag(element);

			if (fCurrentDefinition != null) {
				readElementChildren(element);
			}
			return true;
		}
		else if (tag.equals(TAG_PARTITION_TYPE)) {
			processPartitionTypeTag(element);
			return true;
		}

		return false;
	}

	private void initCache() {
		if (fContentAssistProcessorDefs == null) {
			readContributions(TAG_CONTENTASSISTPROCESSOR, PL_CONTENTASSISTPROCESSOR);
		}
	}

	/**
	 * Returns all the ContentAssistProcessor definition objects
	 * @return
	 */
	public ContentAssistProcessorDefinition[] getContentAssistProcessorDefinitions() {
		initCache();
		return (fContentAssistProcessorDefs == null ? new ContentAssistProcessorDefinition[0] : 
		(ContentAssistProcessorDefinition[])fContentAssistProcessorDefs.toArray(new ContentAssistProcessorDefinition[fContentAssistProcessorDefs.size()]));
	}

	/**
	 * Returns all the ContentAssistProcessor definition objects valid for partitionType
	 *
	 * @param partitionType
	 * @return if partitionType is null, null is returned 
	 */
	public ContentAssistProcessorDefinition[] getContentAssistProcessorDefinitions(String partitionType) {
		if (partitionType == null) {
			return null;
		}

		ContentAssistProcessorDefinition[] allDefs = getContentAssistProcessorDefinitions();
		List defs = new ArrayList();
		List lastDefs = new ArrayList();

		for (int i = 0; i < allDefs.length; ++i) {
			List partitions = (List) allDefs[i].getPartitionTypes();
			if (partitions != null) {
				if (partitions.isEmpty()) {
					lastDefs.add(allDefs[i]);
				}
				else {
					int j = 0; 
					boolean added = false; 
					while (j < partitions.size() && !added) {
						if (partitionType.equals(partitions.get(j))) {
							defs.add(allDefs[i]);
							added = true;
						}
						else {
							++j;
						}
					}
				}
			}
		}
		defs.addAll(lastDefs);

		return (ContentAssistProcessorDefinition[]) defs.toArray(new ContentAssistProcessorDefinition[defs.size()]);
	}
	
	/**
	 * @param tag
	 * @param extensionPoint
	 */
	protected void readContributions(String tag, String extensionPoint) {
		targetContributionTag = tag;
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		readRegistry(registry, XmlEditorPlugin.PLUGIN_ID, extensionPoint);
	}	

	/**
	 * Override to improve sorting
	 */
	protected IExtension[] orderExtensions(IExtension[] extensions) {
		Object[] sorted = createSorter().sort(extensions);
		IExtension[] sortedExtension = new IExtension[sorted.length];
		System.arraycopy(sorted, 0, sortedExtension, 0, sorted.length);
		return sortedExtension;
	}
	
	protected Sorter createSorter() {
		return new Sorter() {
			public boolean compare(Object extension1, Object extension2) {
				String s1 = ((IExtension) extension1).getUniqueIdentifier().toUpperCase();
				String s2 = ((IExtension) extension2).getUniqueIdentifier().toUpperCase();
				return s2.compareTo(s1) > 0;
			}
		};
	}

	protected void logUnknownElement(IConfigurationElement element) {
		//do nothing
	}
}
