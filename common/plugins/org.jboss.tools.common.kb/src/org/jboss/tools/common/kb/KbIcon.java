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

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

/**
 * @author igels
 */
public class KbIcon implements Serializable {

	private static final long serialVersionUID = 3762136564046244137L;

	private static Map<String,KbIcon> allObjects = new Hashtable<String,KbIcon>();

	private final String imageType;
	private String imagePath;
	private transient Image image = null;

	/**
	 * 
	 */
	private KbIcon(String imageType, String imagePath) {
		this.imageType = imageType;
		this.imagePath = imagePath;

		allObjects.put(imageType, this);		
	}

	public static final KbIcon ENUM_ITEM = new KbIcon("Enumeration item icon", "/icons/proposals/enum_item.gif");

	public static final KbIcon HTML_ATTRIBUTE = new KbIcon("HTML requared attribute icon", "/icons/proposals/html_attribute.gif");
	public static final KbIcon HTML_ATTRIBUTE_OPTIONAL = new KbIcon("HTML optional attribute icon", "/icons/proposals/html_attribute_optional.gif");
	public static final KbIcon HTML_TAG = new KbIcon("HTML tag icon","/icons/proposals/html_tag.gif");

	public static final KbIcon TLD_ATTRIBUTE = new KbIcon("TLD requared attribute icon", "/icons/proposals/tld_attribute.gif");
	public static final KbIcon TLD_ATTRIBUTE_OPTIONAL = new KbIcon("TLD optional attribute icon", "/icons/proposals/tld_attribute_optional.gif");
	public static final KbIcon TLD_TAG = new KbIcon("TLD tag icon", "/icons/proposals/tld_tag.gif");

	public static final KbIcon XML_ATTRIBUTE = new KbIcon("XML requared attribute icon", "/icons/proposals/xml_attribute.gif");
	public static final KbIcon XML_ATTRIBUTE_OPTIONAL = new KbIcon("XML optional attribute icon", "/icons/proposals/xml_attribute_optional.gif");
	public static final KbIcon XML_TAG = new KbIcon("XML tag icon","/icons/proposals/xml_tag.gif");

	public static final KbIcon JSP_DIRECTIVE_ATTRIBUTE = new KbIcon("JSP directive requared attribute icon", "/icons/proposals/jsp_directive_attr.gif");
	public static final KbIcon JSP_DIRECTIVE_ATTRIBUTE_OPTIONAL = new KbIcon("JSP directive optional attribute icon","/icons/proposals/jsp_directive_attr_optional.gif");
	public static final KbIcon JSP_DIRECTIVE = new KbIcon("JSP directive icon", "/icons/proposals/jsp_directive.gif");

	public Image getImage() {
		if (image == null) 
			image = new Image(Display.getCurrent(), KbProposal.class.getResourceAsStream(imagePath));
		
		return image;
	}

	public String toString() {
		return imageType;
	}

	private Object readResolve() throws ObjectStreamException {
		return allObjects.get(imageType);
	}
}