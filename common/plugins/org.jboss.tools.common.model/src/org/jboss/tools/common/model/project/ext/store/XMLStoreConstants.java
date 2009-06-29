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
package org.jboss.tools.common.model.project.ext.store;

import org.jboss.tools.common.model.XModelObjectConstants;

/**
 * @author Viacheslav Kabanovich
 */
public interface XMLStoreConstants {
	public String TAG_VALUE_INFO = "value-info"; //$NON-NLS-1$
	public String TAG_ID = "id"; //$NON-NLS-1$
	public String TAG_ENTRY = "entry"; //$NON-NLS-1$

	public String CLS_XML = "xml"; //$NON-NLS-1$
	public String CLS_MODEL_OBJECT = "model-object"; //$NON-NLS-1$
	public String CLS_FIELD = "field"; //$NON-NLS-1$
	public String CLS_STRING = "string"; //$NON-NLS-1$
	public String CLS_TYPE = "type"; //$NON-NLS-1$
	public String CLS_METHOD = "method"; //$NON-NLS-1$
	

	public String ATTR_VALUE = "value"; //$NON-NLS-1$
	public String ATTR_PATH = "path"; //$NON-NLS-1$
	public String ATTR_CLASS = "class"; //$NON-NLS-1$
	public String ATTR_NAME = XModelObjectConstants.ATTR_NAME;
	public String ATTR_PROJECT = "project"; //$NON-NLS-1$
	public String ATTR_TYPE = "type"; //$NON-NLS-1$
	public String ATTR_PARAMS = "params"; //$NON-NLS-1$

	public String KEY_MODEL_OBJECT = "model-object"; //$NON-NLS-1$
	
}
