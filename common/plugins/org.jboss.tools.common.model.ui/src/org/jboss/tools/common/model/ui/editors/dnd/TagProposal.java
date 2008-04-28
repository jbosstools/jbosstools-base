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
package org.jboss.tools.common.model.ui.editors.dnd;

public class TagProposal {
	
	public static final String EMPTY_PREFIX = "";
	public static final String EMPTY_URI = "";
	
	public static final IAttributeValueLoader EMPTY_ATTRIBUTE_VALUE_LOADER = new IAttributeValueLoader() {
		public void fillTagAttributes(IDropWizardModel model) {
			// do nothing
		}
	};
	
	String uri;
	String libraryVersion = "";
	String prefix;
	String name;
	IAttributeValueLoader attributeValueLoader =  EMPTY_ATTRIBUTE_VALUE_LOADER;
	
	/**
	 * TagProposal with empty attribute value loader  
	 *
	 */
	public TagProposal(
		String uri, 
		String prefix, 
		String name
	) {
		this(uri, "", prefix, name);
	}

	public TagProposal(
			String uri,
			String libraryVersion,
			String prefix, 
			String name
		) {
			this.uri = uri;
			this.libraryVersion = (libraryVersion == null) ? "" : libraryVersion;
			this.prefix = prefix;
			this.name = name;
		}

	/**
	 * 
	 *
	 */
	public TagProposal(
		String uri, 
		String prefix, 
		String name,
		IAttributeValueLoader loader
	) {
		this.uri = uri;
		this.prefix = prefix;
		this.name = name;
		this.attributeValueLoader = loader;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getPrefix() {
		return prefix;
	}

	/**
	 * 
	 * @return
	 */
	public String getUri() {
		return uri;
	}
	
	public String getLibraryVersion() {
		return libraryVersion;
	}
	
	public IAttributeValueLoader getAttributesValueLoader() {
		return attributeValueLoader;
	}
}
