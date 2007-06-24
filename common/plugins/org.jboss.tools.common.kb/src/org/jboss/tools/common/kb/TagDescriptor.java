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
package org.jboss.tools.common.kb;

import java.util.ArrayList;
import java.util.List;

/**
 * Describes tag (ElementType from Schema).
 * @author Igels
 */
public class TagDescriptor {

	private boolean hasBody = false;
	private boolean hasClosingTag = false;
	private String tagName;
	private String prefix;
	private List<AttributeDescriptor> attributesDescriptors = new ArrayList<AttributeDescriptor>();

	/**
     * @return
     */
    public boolean hasBody() {
        return hasBody;
    }

    /**
     * @return
     */
    public boolean hasClosingTag() {
        return hasClosingTag;
    }

    /**
     * @param b
     */
    public void setBody(boolean b) {
        hasBody = b;
    }

    /**
     * @param b
     */
    public void setClosingTag(boolean b) {
        hasClosingTag = b;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
    	StringBuffer sb = new StringBuffer();
    	sb.append("[tagName=")
    		.append(tagName)
    		.append("; hasBody=")
    		.append(hasBody)
    		.append("; hasClosingTag=")
    		.append(hasClosingTag)
    		.append(";]");
        return sb.toString();
    }

    /**
     * @return
     */
    public String getTagName() {
        return tagName;
    }

    /**
     * @param name
     */
    public void setTagName(String name) {
        tagName = name;
    }

    /**
     * @return
     */
    public String getPrefix() {
        return prefix;
    }

    /**
     * @param string
     */
    public void setPrefix(String string) {
        prefix = string;
    }

	/**
	 * @return
	 */
	public String getFullName() {
		if(prefix == null) {
			return tagName;
		}
		return new StringBuffer().append(prefix).append(KbQuery.PREFIX_SEPARATOR).append(tagName).toString();
	}

	/**
	 * 
	 * @param descriptors
	 */
	public void setAttributesDescriptors(List<AttributeDescriptor> descriptors) {
		attributesDescriptors = descriptors;
	}

	/**
	 * 
	 * @return
	 */
	public List getAttributesDescriptors() {
		return attributesDescriptors;
	}

	/**
	 * 
	 * @param attributeName
	 * @return
	 */
	public AttributeDescriptor getAttributeDescriptor(String attributeName) {
		for(int i=0; i<attributesDescriptors.size(); i++) {
			AttributeDescriptor descriptor = (AttributeDescriptor)attributesDescriptors.get(i);
			if(attributeName.equals(descriptor.getName())) {
				return descriptor;
			}
		}
		return null;
	}

	/**
	 * 
	 * @param descriptor
	 */
	public void addAttributeDescriptor(AttributeDescriptor descriptor) {
		attributesDescriptors.add(descriptor);
	}

	/**
	 * 
	 * @param descriptor
	 */
	public void removeAttributeDescriptor(AttributeDescriptor descriptor) {
		attributesDescriptors.remove(descriptor);
	}

	public KbProposal generateProposal() {
		KbProposal proposal = new KbProposal();

		String endTag = hasBody()?"":" /";
		StringBuffer lb = new StringBuffer();
		if(prefix!=null && prefix.length()>0) {
			lb.append(prefix);
			lb.append(KbQuery.PREFIX_SEPARATOR);
		}
		lb.append(tagName);

		proposal.setLabel(lb.toString());

		List attributeList = getAttributesDescriptors();
		StringBuffer attributes = new StringBuffer();
		for(int i=0; i<attributeList.size(); i++) {
			AttributeDescriptor attribute = (AttributeDescriptor)attributeList.get(i);
			if(attribute.isRequired()) {
				attributes.append(" ").append(attribute.getName()).append("=\"\"");
			}
		}
		lb.append(attributes);
		lb.append(endTag);

		proposal.setReplacementString(lb.toString());
		int position = proposal.getReplacementString().indexOf('"');
		if(position!=-1) {
			position ++;
		} else {
			position = proposal.getReplacementString().length();
		}
		proposal.setPosition(position);
		proposal.setIcon(KbIcon.TLD_TAG);

		return proposal;
	}
}