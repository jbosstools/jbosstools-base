/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.util.xpl.RegistryReader;

/**
 * @author Igels
 */
public class HyperlinkPartitionerBuilder extends RegistryReader {

	public static final String PL_HYPERLINK_PARTITIONER = "hyperlinkPartitioner";

	public static final String PLUGIN_ID = ExtensionsPlugin.PLUGIN_ID;

	public static final String TAG_HYPERLINK_PARTITIONER = "hyperlinkPartitioner";
	public static final String TAG_CONTENT_TYPE = "contentType";
	public static final String TAG_PARTITION_TYPE = "partitionType";
	public static final String TAG_AXIS = "axis";

	public static final String ATT_ID = "id";
	public static final String ATT_CLASS = "class";
	public static final String ATT_PATH = "path";
	public static final String ATT_IGNORE_CASE = "ignoreCase";

	private static HyperlinkPartitionerBuilder fInstance;
	private ArrayList fPartitionerDefs;
	private HyperlinkPartitionerDefinition fCurrentHyperlinkPartitionerDefinition = null;
	private String fCurrentContentType;
	private String fCurrentPartitionType;

	protected String targetContributionTag;

	/**
	 * returns singleton instance of HyperlinkPartitionerBuilder
	 * 
	 * @return HyperlinkPartitionerBuilder
	 */
	public synchronized static HyperlinkPartitionerBuilder getInstance() {
		if (fInstance == null) {
			fInstance = new HyperlinkPartitionerBuilder();
		}
		return fInstance;
	}

	private HyperlinkPartitionerBuilder() {
	    super();
	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.internal.extension.RegistryReader#readElement(org.eclipse.core.runtime.IConfigurationElement)
	 */
	protected boolean readElement(IConfigurationElement element) {
		String tag = element.getName();

		if(tag.equals(targetContributionTag)) {
		    processHyperlinkPartitionerTag(element);
			// make sure processing of current partition tag resulted in a current partition definition
			// before continue reading the children
			if (fCurrentHyperlinkPartitionerDefinition != null) {
				readElementChildren(element);
			}
			return true;
		} else if(tag.equals(TAG_CONTENT_TYPE)) {
			processContentTypeTag(element);
			// make sure processing of current content type resulted in a valid content type
			// before reading the children
			if (fCurrentContentType != null) {
				readElementChildren(element);
			}
			return true;
		} else if(tag.equals(TAG_PARTITION_TYPE)) {
			processPartitionTypeTag(element);
			// make sure processing of current partition type resulted in a valid partition type
			// before reading the children
			if (fCurrentPartitionType != null) {
				readElementChildren(element);
			}
			return true;
		} else if(tag.equals(TAG_AXIS)) {
			processAxisTag(element);
			return true;
		}

		return false;
	}

	/**
	 * Processes element which should be a configuration element specifying an
	 * open on object.  Creates a new open on partitioner definition object and
	 * adds it to the list of open on partitioner definition objects
	 * 
	 * @param element hyperlink partitioner configuration element
	 */
	private void processHyperlinkPartitionerTag(IConfigurationElement element) {
		String theId = getId(element);
		String theClass = getHyperlinkPartitionerClass(element);

		if(theId != null && theClass != null) {
			// start building new HyperlinkPartitionerDefinition
			fCurrentHyperlinkPartitionerDefinition = new HyperlinkPartitionerDefinition(theId, theClass, element);

			// create a new list of open on partitioner definitions if it hasn't been created yet
			if (fPartitionerDefs == null) {
			    fPartitionerDefs = new ArrayList();
			}
			fPartitionerDefs.add(fCurrentHyperlinkPartitionerDefinition);
		} else {
		    fCurrentHyperlinkPartitionerDefinition = null;
		}
	}

	/**
	 * Processes element which should be a configuration element specifying a content
	 * type for the current open on partitioner tag. Assumes that there is a valid current open
	 * on partitioner definition object.
	 * 
	 * @param element contentType configuration element
	 */
	private void processContentTypeTag(IConfigurationElement element) {
		// add to current hyperlinkDefinition
		String theId = getId(element);

		if (theId != null) {
			fCurrentContentType = theId;
			fCurrentHyperlinkPartitionerDefinition.addContentTypeId(fCurrentContentType);
		} else {
			fCurrentContentType = null;
		}
	}

	/**
	 * Processes element which should be a configuration element specifying a partition
	 * type for the current open on partitioner/content type tag. Assumes that there is a valid
	 * current open on partitioner/content type tag.
	 * 
	 * @param element partitionType configuration element
	 */
	private void processPartitionTypeTag(IConfigurationElement element) {
		// add to current hyperlinkPartitionerDefinition/contentType
		String theId = getId(element);

		if (theId != null) {
		    fCurrentPartitionType = theId;
		    fCurrentHyperlinkPartitionerDefinition.addPartitionType(fCurrentContentType, theId);
		} else {
		    fCurrentPartitionType = null;
		}
	}

	/**
	 * Processes element which should be a configuration element specifying a axis
	 * for the current open on partitioner/partition type tag. Assumes that there is a valid
	 * current open on partitioner/partition type tag.
	 * 
	 * @param element axis configuration element
	 */
	private void processAxisTag(IConfigurationElement element) {
		// add to current hyperlinkPartitionerDefinition/contentType
		String thePath = getPath(element);
		boolean ignoreCase = isIgnoreCase(element);

		if (thePath != null) {
		    fCurrentHyperlinkPartitionerDefinition.addAxis(fCurrentContentType, fCurrentPartitionType, thePath, ignoreCase);
		}
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

	/**
	 * Returns the name of the part PATH attribute that is expected
	 * in the target extension.
	 * 
	 * @param element
	 * @return String
	 */
	protected String getPath(IConfigurationElement element) {
		String value = element.getAttribute(ATT_PATH);
		return value;
	}

	protected boolean isIgnoreCase(IConfigurationElement element) {
		String value = element.getAttribute(ATT_IGNORE_CASE);
		if("true".equals(value)) {
		    return true;
		}
		return false;
	}

	protected String getHyperlinkPartitionerClass(IConfigurationElement element) {
		String value = element.getAttribute(ATT_CLASS);
		return value;
	}

	private void initCache() {
	    if(fPartitionerDefs==null) {
			readContributions(TAG_HYPERLINK_PARTITIONER, PL_HYPERLINK_PARTITIONER);			
	    }
	}

	/**
	 * Reads the contributions from the registry for the provided workbench
	 * part and the provided extension point ID.
	 * @param tag
	 * @param extensionPoint
	 */
	protected void readContributions(String tag, String extensionPoint) {
		targetContributionTag = tag;
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		readRegistry(registry, PLUGIN_ID, extensionPoint);
	}

	/**
	 * Returns all the open on definition objects
	 * @return
	 */
	public HyperlinkPartitionerDefinition[] getHyperlinkPartitionerDefinitions() {
	    initCache();
	    if(fPartitionerDefs==null) {
	        return new HyperlinkPartitionerDefinition[0];
	    }
		return (HyperlinkPartitionerDefinition[])fPartitionerDefs.toArray(new HyperlinkPartitionerDefinition[fPartitionerDefs.size()]);
	}

	/**
	 * Returns all the open on partitioner definition objects valid for contentType/partitionType/axis
	 * @param contentType
	 * @param partitionType
	 * @param axis 
	 * @return HyperlinkPartitionerDefinition[] 
	 */
	public HyperlinkPartitionerDefinition[] getHyperlinkPartitionerDefinitions(String contentType, String partitionType, String axis) {
		if (contentType == null || partitionType == null) {
			// should not be able to define an hyperlink without a content type
			return null;
		}

		// entire list of hyperlink definition objects
		HyperlinkPartitionerDefinition[] allDefs = getHyperlinkPartitionerDefinitions();
		// current list of open on definitions valid for contentType/partitionType/axis
		List defs = new ArrayList();

		for(int i=0; i<allDefs.length; i++) {
			// for each one check if it contains contentType
			List contentTypes = allDefs[i].getContentTypes();
			for(int j=0; j<contentTypes.size(); j++) {
			    HyperlinkPartitionerDefinition.ContentType cType = (HyperlinkPartitionerDefinition.ContentType)contentTypes.get(j);
			    if(contentType.equals(cType.getId())) {
				    HyperlinkPartitionerDefinition.PartitionType pType = cType.getPartitionType(partitionType);
				    if(pType!=null && pType.containtsAxis(axis)) {
			            defs.add(allDefs[i]); 
				    }
			    }
			}
		}

		// return the list
		return (HyperlinkPartitionerDefinition[]) defs.toArray(new HyperlinkPartitionerDefinition[defs.size()]);
	}
}