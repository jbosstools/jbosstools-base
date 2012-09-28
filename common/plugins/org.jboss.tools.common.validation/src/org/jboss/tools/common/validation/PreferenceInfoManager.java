/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Preference Info Manager store info from validators and returns it to Quick fixes 
 * @author Daniel Azarov
 */
public class PreferenceInfoManager {
	private static Map<String, IPreferenceInfo> infos = Collections.synchronizedMap(new HashMap<String, IPreferenceInfo>());
	
	/*
	 * register IPreferenceInfo for problemType
	 * this method is designed to be called from validator
	 */
	public static void register(String problemType, IPreferenceInfo info){
		if(!infos.containsKey(problemType)){
			infos.put(problemType, info);
		}
	}
	
	/*
	 * returns IPreferenceInfo for problemType
	 */
	public static IPreferenceInfo getPreferenceInfo(String problemType){
		if(problemType == null){
			problemType = ValidationErrorManager.DEFAULT_VALIDATION_MARKER;
		}
		IPreferenceInfo info = infos.get(problemType);
		if(info == null){
			ValidationContext.loadValidatorByProblemType(problemType);
			info = infos.get(problemType);
		}
		return info;
	}
}
