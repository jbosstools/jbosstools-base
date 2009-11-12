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
package org.jboss.tools.common.model.ui.forms;

import org.jboss.tools.common.meta.key.WizardKeys;

/**
 * @author Igels
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class FormAttributeData implements IFormAttributeData {

	private String name; // (non-translatable)
	private ILayoutDataFactory layoutDataFactory;
	private String wraperClassName; // (non-translatable)
	private int width;
	private String displayName; // (translatable)

	/**
	 * 
	 * @param name (non-translatable)
	 * @param layoutDataFactory
	 * @param wraperClassName (non-translatable)
	 * @param width
	 * @param displayname (translatable)
	 */
	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName, int width, String displayname) {
		this.name = name;
		if(layoutDataFactory == null) layoutDataFactory = LayoutDataFactory.getInstance();
		this.layoutDataFactory = layoutDataFactory;
		this.wraperClassName = wraperClassName;
		this.width = width;
		this.displayName = displayname;
	}

	/**
	 * 
	 * @param name (non-translatable)
	 * @param layoutDataFactory
	 * @param wraperClassName (non-translatable)
	 * @param width
	 */
	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName, int width) {
		this(name, layoutDataFactory, wraperClassName, width, null);
	}

	/**
	 * 
	 * @param name (non-translatable)
	 * @param layoutDataFactory
	 * @param wraperClassName (non-translatable)
	 */
	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName) {
		this(name, layoutDataFactory, wraperClassName, 0);
	}

	/**
	 * 
	 * @param name (non-translatable)
	 * @param layoutDataFactory
	 */
	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory) {
		this(name, layoutDataFactory, null, 0);
	}
	
	/**
	 * 
	 * @param name (non-translatable)
	 * @param width
	 */
	public FormAttributeData(String name, int width) {
		this(name, LayoutDataFactory.getInstance(), null, width);
	}

	/**
	 * 
	 * @param name (non-translatable)
	 * @param width
	 * @param displayName (translatable)
	 */
	public FormAttributeData(String name, int width, String displayName) {
		this(name, LayoutDataFactory.getInstance(), null, width, displayName);
	}

	/**
	 * 
	 * @param name (non-translatable)
	 */
	public FormAttributeData(String name) {
		this(name, LayoutDataFactory.getInstance(), null, 0);
	}

    public String getName() {
        return name;
    }

	/*
	 * For use only as table column name
	 */
	public String getDisplayName() {
		return (displayName == null) ? WizardKeys.toDisplayName(name) : displayName;
	}

    public ILayoutDataFactory getLayoutDataFactory() {
        return layoutDataFactory;
    }

    public String getWraperClassName() {
    	return wraperClassName;
    }

    public int getWidth() {
        return width;
    }

    public void setWrapperClassName(String c) {
    	wraperClassName = c;
    }
}