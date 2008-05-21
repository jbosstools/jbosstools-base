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
package org.jboss.tools.jst.web.tld;

import java.util.Iterator;
import java.util.List;

public class TaglibData {
	private int id;
	private String uri;
	private String prefix;
	private boolean ns;

	public TaglibData(int id, String uri, String prefix, boolean ns) {
		this.id = id;
		this.uri = uri;
		this.prefix = prefix;
		this.ns = ns;
	}

	public TaglibData(int id, String uri, String prefix) {
		this.id = id;
		this.uri = uri;
		this.prefix = prefix;
		this.ns = true;
	}

	public int getId() {
		return id;
	}

	public String getUri() {
		return uri;
	}

	public String getPrefix() {
		return prefix;
	}

	public boolean isNs() {
		return ns;
	}

	public boolean isEquals(TaglibData another) {
		return this == another ||
			uri.equals(another.getUri()) && prefix.equals(another.getPrefix());
	}

	public boolean inList(List taglibs) {
		if (taglibs == null) {
			return false;
		}
		Iterator iter = taglibs.iterator();
		while (iter.hasNext()) {
			if (isEquals((TaglibData)iter.next())) {
				return true;
			}
		}
		return false;
	}
}