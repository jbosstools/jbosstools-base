package org.jboss.tools.common.jdt.debug.ui.preferences;

import org.eclipse.jface.bindings.keys.KeySequence;
import org.eclipse.jface.bindings.keys.ParseException;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;

public class RemoteDebug {

	private int id;
	private String key;
	private String description;
	private String port;
	private boolean show;
	private String formatedKey;
	
	public RemoteDebug(int id, String key, String description, String port, boolean show) {
		this.id = id;
		this.key = key;
		this.description = description;
		this.port = port;
		this.show = show;
		this.formatedKey = format(key);
	}

	private String format(String sequence) {
		try {
			KeySequence keySequence = KeySequence.getInstance(sequence);
			return keySequence.format();
		} catch (ParseException e) {
			RemoteDebugUIActivator.log(e);
			return sequence;
		}
	}
	
	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getPort() {
		return port;
	}

	public void setPort(String port) {
		this.port = port;
	}

	public boolean isShow() {
		return show;
	}

	public void setShow(boolean show) {
		this.show = show;
	}

	public boolean isValid() {
		int port = 0;
		try {
			port = new Integer(this.port);
		} catch (NumberFormatException e) {
			// ignore
		}
		return port > 0;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RemoteDebug other = (RemoteDebug) obj;
		if (id != other.id)
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "RemoteDebug [id=" + id + ", key=" + key + ", description="
				+ description + ", port=" + port + ", show=" + show + "]";
	}

	public String getKey(boolean formated) {
		if (formated) {
			return formatedKey;
		}
		return key;
	}

}
