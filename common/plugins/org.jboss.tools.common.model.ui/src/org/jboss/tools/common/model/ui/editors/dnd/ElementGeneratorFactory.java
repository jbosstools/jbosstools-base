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

import java.util.HashMap;
import java.util.Map;

public class ElementGeneratorFactory {
	
	private static final ElementGeneratorFactory INSTANCE
		= new ElementGeneratorFactory(); 
		
	public static final IElementGenerator DEFAULT_ELEMENT_GENERATOR = new DefaultElementGenerator();
	public static Map<String,Class> generatorMap = new HashMap<String,Class>(); 

	static {
		generatorMap.put(DropURI.HTML_4_0_URI,HtmlElementDropGenerator.class);
		generatorMap.put(DropURI.JSF_CORE_URI,DefaultElementGenerator.class);
		generatorMap.put(DropURI.JSF_HTML_URI,DefaultElementGenerator.class);
		generatorMap.put(DropURI.JSP_URI,DefaultElementGenerator.class);
		generatorMap.put(DropURI.STRUTS_HTML_URI,DefaultElementGenerator.class);		
	}
	
	public static ElementGeneratorFactory getInstance() {
		return INSTANCE;
	}
	
    private ElementGeneratorFactory() {
	}
    
    public IElementGenerator getElementGenerator(String uri) {
    	IElementGenerator fInstance = null;
		try {
			Class fClass = (Class)generatorMap.get(uri);
			fInstance = (IElementGenerator)fClass.newInstance();
		} catch (Exception e) {
			return DEFAULT_ELEMENT_GENERATOR;
		}
		return fInstance;
    }
}
