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
package org.jboss.tools.common.model.ui.templates.model;

/**
 * @author au
 */
public interface MetaTemplateConstants {
    // Extention point id
    public static final String EXTENSION_POINT = "org.jboss.tools.common.model.ui.metaTemplates";
    // Extention point tags
    public static final String PREFIX = "%";
    public static final String INCLUDE = "include";
    public static final String FILE = "file";
    public static final String TRANSLATIONS = "translations";
	public static final String META_TEMPLATE_GROUPS = "meta-template-groups";
	public static final String META_TEMPLATE_GROUP = "meta-templates";
	public static final String META_TEMPLATE = "meta-template";
	public static final String SUPER_CLASS = "super-class";
	public static final String INTERFACE = "interface";
	// MetaTemplateGroup
	public static final String URI = "uri";
	// MetaTemplate
	public static final String AXIS = "axis"; 
	public static final String DISPLAY_NAME = "displayName"; 
	public static final String ENTITY = "xEntity"; 
	// TetaTemplateClass
	public static final String NAME = "name";	
}
