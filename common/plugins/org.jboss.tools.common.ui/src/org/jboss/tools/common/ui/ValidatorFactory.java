/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.osgi.util.NLS;

/**
 * @author Alexey Kazakov
 */
public class ValidatorFactory {

	public static final String DEFAULT_SOURCE_LEVEL = CompilerOptions.VERSION_1_5;
	public static final String DEFAULT_COMPLIANCE_LEVEL = DEFAULT_SOURCE_LEVEL;

	static public Map<String, IValidator> validators = new HashMap<String, IValidator>();

	static public final Map<String, IStatus> NO_ERRORS = Collections
			.unmodifiableMap(new HashMap<String, IStatus>());

	static public IValidator NO_ERRORS_VALIDATOR = new IValidator() {
		public Map<String, IStatus> validate(Object value, Object context) {
			return NO_ERRORS;
		}
	};

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static IValidator getValidator(String id) {
		IValidator validator = validators.get(id);
		return validator == null ? NO_ERRORS_VALIDATOR : validator;
	}

	/**
	 * 
	 * @return
	 */
	public static Map<String, IStatus> createErrorMap() {
		return new HashMap<String, IStatus>();
	}

	/**
	 * 
	 * @param text
	 * @return
	 */
	public static Map<String, IStatus> createErrormessage(IStatus message) {
		Map<String, IStatus> map = createErrorMap();
		map.put(IValidator.DEFAULT_ERROR, message);
		return map;
	}

	/**
	 * 
	 * @param text
	 * @return
	 */
	public static Map<String, IStatus> createErrormessage(String propertyName,
			IStatus message) {
		Map<String, IStatus> map = createErrorMap();
		map.put(propertyName, message);
		return map;
	}

	public static final IValidator FILE_SYSTEM_FOLDER_EXISTS = new IValidator() {

		public Map<String, IStatus> validate(Object value, Object context) {
			if (value == null)
				throw new IllegalArgumentException(
						CommonUIMessages.VALIDATOR_FACTORY_PATH_TO_A_FOLDER_CANNOT_BE_NULL);
			String folderPath = value.toString();
			File folder = new File(folderPath);

			if (!folder.exists())
				return createErrormessage(new Status(IStatus.ERROR, CommonUIPlugin.PLUGIN_ID, 
						NLS.bind(CommonUIMessages.VALIDATOR_FACTORY_FOLDER_DOES_NOT_EXIST,
						folderPath)));
			if (!folder.isDirectory())
				return createErrormessage(new Status(IStatus.ERROR, CommonUIPlugin.PLUGIN_ID, 
						NLS.bind(CommonUIMessages.VALIDATOR_FACTORY_PATH_POINTS_TO_FILE,
						folderPath)));
			return NO_ERRORS;
		}
	};

	/**
	 * 
	 * @param jProject
	 * @return java project's CompilerSourceLevel or default one.
	 */
	public static String getCompilerSourceLevel(IJavaProject jProject){
		if (jProject == null){
			return DEFAULT_SOURCE_LEVEL;
		}
		String sourceLevel = jProject.getOption(JavaCore.COMPILER_SOURCE, true);
		return sourceLevel != null ? sourceLevel : DEFAULT_SOURCE_LEVEL;
	}
	
	/**
	 * 
	 * @param jProject
	 * @return java project's CompilerComplianceLevel or default one.
	 */
	public static String getCompilerComplianceLevel(IJavaProject jProject){
		if (jProject == null){
			return DEFAULT_COMPLIANCE_LEVEL;
		}
		String compliance = jProject.getOption(JavaCore.COMPILER_COMPLIANCE, true);
		return compliance != null ? compliance : DEFAULT_SOURCE_LEVEL;
	}
}