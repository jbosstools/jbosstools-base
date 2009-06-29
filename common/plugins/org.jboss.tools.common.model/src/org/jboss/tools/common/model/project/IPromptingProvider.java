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
package org.jboss.tools.common.model.project;

import java.util.*;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObjectConstants;

public interface IPromptingProvider {
	static String ERROR = "error"; //$NON-NLS-1$
	static List<Object> EMPTY_LIST = Collections.unmodifiableList(new ArrayList<Object>());

	static String PROPERTY_TYPE = "propertyType"; //$NON-NLS-1$
	static String FILE = "file"; //$NON-NLS-1$
	static String KEY = "key"; //$NON-NLS-1$
	static String MODULE = "module"; //$NON-NLS-1$
	static String ACTION = "action"; //$NON-NLS-1$
	static String TYPE = "type"; //$NON-NLS-1$
	static String PROPERTY = "property"; //$NON-NLS-1$
	static String MODEL_OBJECT_PATH = "model-path"; //$NON-NLS-1$
	static String NAME = XModelObjectConstants.ATTR_NAME;
	static String ATTRIBUTE = "attribute"; //$NON-NLS-1$
	static String LOCALE = "locale"; //$NON-NLS-1$
	
	public boolean isSupporting(String id);
	public List getList(XModel model, String id, String prefix, Properties properties);
}
