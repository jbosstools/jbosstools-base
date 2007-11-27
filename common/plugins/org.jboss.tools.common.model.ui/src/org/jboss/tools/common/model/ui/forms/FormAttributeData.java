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

/**
 * @author Igels
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class FormAttributeData implements IFormAttributeData {

	private String name;
	private ILayoutDataFactory layoutDataFactory;
	private String wraperClassName;
	private int width;
	private String displayName;

	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName, int width, String displayname) {
		this.name = name;
		this.layoutDataFactory = layoutDataFactory;
		this.wraperClassName = wraperClassName;
		this.width = width;
		this.displayName = displayname;
	}

	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName, int width) {
		this(name, layoutDataFactory, wraperClassName, width, null);
	}

	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory, String wraperClassName) {
		this(name, layoutDataFactory, wraperClassName, 0);
	}

	public FormAttributeData(String name, ILayoutDataFactory layoutDataFactory) {
		this(name, layoutDataFactory, null, 0);
	}

	public FormAttributeData(String name, int width) {
		this(name, LayoutDataFactory.getInstance(), null, width);
	}

	public FormAttributeData(String name, int width, String displayName) {
		this(name, LayoutDataFactory.getInstance(), null, width, displayName);
	}

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
		return (displayName == null) ? name : displayName;
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
}