/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties.internal;

import java.util.Arrays;

/**
 *  <i>Simple</i> Hierarchical version model.  
 */
public class SimpleHierarchicalVersion {
	
	private String internalVersion;
	
	public SimpleHierarchicalVersion(String version) {
		internalVersion = version;
	}
	
	/**
	 * Returns a parent {@link SimpleHierarchicalVersion} for this instance, corresponding to one level up in the version hierarchy. 
	 * For instance, it'll return : 
	 * <ul>
	 * <li>1.2.3.GA for 1.2.3.GA-123456-23455</li>	 
	 * <li>1.2.3 for 1.2.3.GA</li>	 
	 * <li>1.2 for 1.2.3</li>	 
	 * <li>1 for 1.2</li>	 
	 * <li><code>null</code> for 1</li>	 
	 * </ul>
	 * 
	 * @return the parent {@link SimpleHierarchicalVersion} for this instance, or <code>null</code> if it has no parent. 
	 */
	public SimpleHierarchicalVersion getParentVersion() {
		
		String parentVersion = getOneVersionUp(internalVersion);
		if (parentVersion == null || internalVersion.equals(parentVersion)) {
			return null;
		}
		return new SimpleHierarchicalVersion(parentVersion);
	}
	
	static String getOneVersionUp(String version) {
		if (version == null) {
			 return null;
		}
		String[] segments = version.split("\\.");
		int newLength = 0;
		int curLength = segments.length;
		
		if (curLength > 3) {
			String qualifier = segments[3];
			int dashIndex = qualifier.indexOf('-');
			if ( dashIndex > 0) {
			  segments[3] = qualifier.substring(0, dashIndex);
			  newLength = 4;
			} else {
			  newLength = curLength > 4? 4 : 3;
			}
		} else {
			newLength = segments.length -1;
		}
		if (newLength > 0) {
			return join(Arrays.copyOf(segments, newLength));
		}
		return null;
	}

	@Override
	public String toString() {
		return internalVersion;
	}

	private static String join(String...segments) {
		if (segments.length == 0) {
			return null;
		}
		if (segments.length == 1) {
			return segments[0];
		}
		boolean addSeparator = false;
		StringBuilder sb = new StringBuilder();
		for (String s : segments) {
			if (addSeparator) {
				sb.append('.');
			}
			sb.append(s);
			addSeparator = true;
		}
		return sb.toString();
	}
}
