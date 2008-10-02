/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.internal.core.parser.token;

import java.util.ArrayList;
import java.util.Properties;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ParamUtil {
	static String PARAM_HISTORY = "(_level";
	
	static class ParamHistory extends ArrayList<String>{
		private static final long serialVersionUID = 1L;
	}

	public static boolean isParamHistoryEmpty(Properties context) {
		ParamHistory i = (ParamHistory)(context.get(PARAM_HISTORY));
		return i == null || i.size() == 0;
	}

	public static void cleanParamHistory(Properties context) {
		context.remove(PARAM_HISTORY);
	}

	public static void createParamHistory(Properties context) {
		context.put(PARAM_HISTORY, new ParamHistory());
	}

	public static boolean isMethodParamContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get(PARAM_HISTORY));
		return i != null && i.size() > 0 && "params".equals(i.get(i.size() - 1));
	}

	public static boolean isArgContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get(PARAM_HISTORY));
		return i != null && i.size() > 0 && "arg".equals(i.get(i.size() - 1));
	}

	public static boolean isComplexExpressionContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get("(_level"));
		return i != null && i.size() > 0 && "expr".equals(i.get(i.size() - 1));
	}

	public static void closeCurrentParamContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get(PARAM_HISTORY));
		if(i != null && i.size() > 0) {
			i.remove(i.size() - 1);
		}
	}

	public static void openMethodParamContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get("(_level"));
		i.add("params");
	}

	public static void openComplexExpressionContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get("(_level"));
		i.add("expr");
	}

	public static void openArgContext(Properties context) {
		ParamHistory i = (ParamHistory)(context.get("(_level"));
		i.add("arg");
	}


}
