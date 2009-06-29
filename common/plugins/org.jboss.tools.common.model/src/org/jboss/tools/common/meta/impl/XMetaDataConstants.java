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
package org.jboss.tools.common.meta.impl;

import org.jboss.tools.common.model.XModelObjectConstants;

public interface XMetaDataConstants {

    String XMODEL_STRUCTURE = "XModelStructure"; //$NON-NLS-1$
    String XMODEL_ENTITY = "XModelEntity"; //$NON-NLS-1$
    String XENTITY_EXTENTION = "XEntityExtension"; //$NON-NLS-1$
    String XMODEL_ATTRIBUTES = "XModelAttributes"; //$NON-NLS-1$
    String XMODEL_ATTRIBUTE = "XModelAttribute"; //$NON-NLS-1$
    String XMODEL_ATTRIBUTE_REF = "XModelAttributeReference"; //$NON-NLS-1$
    String XMODEL_ACTIONS = "XModelActions"; //$NON-NLS-1$
    String XMODEL_ACTION  = "XModelAction"; //$NON-NLS-1$
    String XMODEL_ACTION_ITEM  = "XActionItem"; //$NON-NLS-1$
    String XMODEL_ACTION_ITEM_REF  = "XActionItemReference"; //$NON-NLS-1$
    String XMODEL_CHILDREN = "XChildrenEntities"; //$NON-NLS-1$
    String XMODEL_CHILD  = "XChildEntity"; //$NON-NLS-1$
    String NAME = XModelObjectConstants.ATTR_NAME;
    String XML_NAME = "xmlname"; //$NON-NLS-1$
    String DISPLAYNAME="displayName"; //$NON-NLS-1$
    String LOADER = "loader"; //$NON-NLS-1$

    String IMPLEMENTING_CLASS = "ImplementingClass"; //$NON-NLS-1$
    String IMPLEMENTATION_LOADING_CLASS = "ImplementationLoadingClass"; //$NON-NLS-1$
    String IMPLEMENTATION_GENERATOR_CLASS = "ImplementationGenerator"; //$NON-NLS-1$
    String OBJECT_EDITOR_CLASSNAME = "ObjectEditorClass"; //$NON-NLS-1$
    String ADOPT_MANAGER_CLASS = "AdoptManagerClass"; //$NON-NLS-1$

    String CONSTRAINT = "Constraint"; //$NON-NLS-1$
    String VALUE = "value"; //$NON-NLS-1$
    String DEFAULT_VALUE = "default"; //$NON-NLS-1$
    String RENDERER = "XEntityRenderer"; //$NON-NLS-1$
    String GROUP_NAME= "groupName"; //$NON-NLS-1$
    String HANDLER_CLASSNAME= "handler"; //$NON-NLS-1$

    String EDITOR = "Editor"; //$NON-NLS-1$
    String EDITOR_CLASSNAME = "class"; //$NON-NLS-1$
    String EDITOR_VIEWERNAME = "viewer"; //$NON-NLS-1$
    String EDITOR_ALLOWS_ANY_VALUE = "allows_any_value"; //$NON-NLS-1$
    String REQUIRED = "required"; //$NON-NLS-1$
    String EDITABLE = "editable"; //$NON-NLS-1$
    String VISIBLE  = "visibility"; //$NON-NLS-1$
    String MAX_COUNT= "maxCount"; //$NON-NLS-1$
    String ENTITY_NAME= "entityName"; //$NON-NLS-1$

    String ENTITY = "entity"; //$NON-NLS-1$

    String ICONS = "ICONS"; //$NON-NLS-1$
    String ICON = "ICON"; //$NON-NLS-1$
    String ICON_PATH = "path"; //$NON-NLS-1$
    String ICON_TYPE = "type"; //$NON-NLS-1$
    String ICON_INFO = "info"; //$NON-NLS-1$

    String MAPPINGS = "MAPPINGS"; //$NON-NLS-1$
    String MAPPING = "MAPPING"; //$NON-NLS-1$
    String PAIR = "PAIR"; //$NON-NLS-1$
    String DEFAULT_KEY = "*"; //$NON-NLS-1$

}

