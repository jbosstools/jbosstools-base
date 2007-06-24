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
package org.jboss.tools.common.kb;

import java.util.Collection;

/**
 * Describes Dinamic Resorce.
 * This resource provides any information from project model.
 * @author igels
 */
public interface KbDinamicResource extends KbResource {

	public static final String BUNDLE_PROPERTY_TYPE = "bundleProperty";
	public static final String BEAN_PROPERTY_TYPE = "beanProperty";
	public static final String BEAN_METHOD_BY_SYGNATURE_TYPE = "beanMethodBySignature";
	public static final String JSP_PATH_TYPE = "jspPath";
	public static final String BUNDLE_NAME_TYPE = "bundleName";
	public static final String VIEW_ACTIONS_TYPE = "viewActions";
	public static final String IMAGE_FILE_TYPE = "file";
	public static final String ENUMERATION_TYPE = "enumeration";
	public static final String JSF_VARIABLES_TYPE = "jsfVariables";
	public static final String FACELETS_JSFC_TYPE = "faceletsJsfCTags";
	public static final String MANAGED_BEAN_NAME_TYPE = "managedBeanName";
	public static final String JSF_ID = "jsfID";
	public static final String TAGLIB_TYPE = "taglib";

	/**
	 * Sets a parameter for resource.
	 * @param name
	 * @param value
	 */
	public void setConstraint(String name, String value);

	/**
	 * Clear every parameter.
	 */
	public void clearConstraints();

	/**
	 * @param query
	 * @return Collection of proposals.
	 */
	public Collection<KbProposal> queryProposal(String query);

	/**
	 * @return Type of Resource.
	 */
	public String getType();
}