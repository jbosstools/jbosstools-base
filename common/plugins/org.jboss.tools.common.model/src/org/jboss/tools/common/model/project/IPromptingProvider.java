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

public interface IPromptingProvider {
	static String ERROR = "error";
	static List<Object> EMPTY_LIST = Collections.unmodifiableList(new ArrayList<Object>());

	static String PROPERTY_TYPE = "propertyType";
	static String FILE = "file";
	static String KEY = "key";
	static String MODULE = "module";
	static String ACTION = "action";
	static String TYPE = "type";
	static String PROPERTY = "property";
	static String MODEL_OBJECT_PATH = "model-path";
	static String NAME = "name";
	static String ATTRIBUTE = "attribute";
	static String LOCALE = "locale";
	
	public boolean isSupporting(String id);
	public List getList(XModel model, String id, String prefix, Properties properties);
}
