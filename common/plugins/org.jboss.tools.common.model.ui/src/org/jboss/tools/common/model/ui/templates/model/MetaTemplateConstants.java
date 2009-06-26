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
    public static final String EXTENSION_POINT = "org.jboss.tools.common.model.ui.metaTemplates"; //$NON-NLS-1$
    // Extention point tags
    public static final String PREFIX = "%"; //$NON-NLS-1$
    public static final String INCLUDE = "include"; //$NON-NLS-1$
    public static final String FILE = "file"; //$NON-NLS-1$
    public static final String TRANSLATIONS = "translations"; //$NON-NLS-1$
	public static final String META_TEMPLATE_GROUPS = "meta-template-groups"; //$NON-NLS-1$
	public static final String META_TEMPLATE_GROUP = "meta-templates"; //$NON-NLS-1$
	public static final String META_TEMPLATE = "meta-template"; //$NON-NLS-1$
	public static final String SUPER_CLASS = "super-class"; //$NON-NLS-1$
	public static final String INTERFACE = "interface"; //$NON-NLS-1$
	// MetaTemplateGroup
	public static final String URI = "uri"; //$NON-NLS-1$
	// MetaTemplate
	public static final String AXIS = "axis";  //$NON-NLS-1$
	public static final String DISPLAY_NAME = "displayName";  //$NON-NLS-1$
	public static final String ENTITY = "xEntity";  //$NON-NLS-1$
	// TetaTemplateClass
	public static final String NAME = "name";	 //$NON-NLS-1$
}
