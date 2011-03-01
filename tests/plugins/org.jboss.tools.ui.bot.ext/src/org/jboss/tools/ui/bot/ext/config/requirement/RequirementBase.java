package org.jboss.tools.ui.bot.ext.config.requirement;

import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;

/**
 * An abstract class for all requirements
 * 
 * @author lzoubek
 * 
 */
public abstract class RequirementBase {
	private int priority = 0;
	protected final Logger log = Logger.getLogger(this.getClass());

	public RequirementBase() {
	}

	private List<RequirementBase> dependsOn;

	/**
	 * gets the list of reqs on which this one depends
	 * 
	 * @return
	 */
	public List<RequirementBase> getDependsOn() {
		if (dependsOn == null) {
			dependsOn = new Vector<RequirementBase>();
		}
		return dependsOn;
	}

	/**
	 * fulfills this requirement. First fulfills the dependent ones, then this.
	 * 
	 * @throws RequirementNotFulfilledException
	 */
	public void fulfill() throws RequirementNotFulfilledException {
		log.info("Fulfilling requirement '" + this.getClass().getName() + "'");
		try {
			if (!getDependsOn().isEmpty()) {
				log.info("Fulfilling dependencies");
				for (RequirementBase dep : getDependsOn()) {
					dep.fulfill();
				}
				log.info("All dependencies fulfilled");
			}
			if (!checkFulfilled()) {
				handle();
				if (!checkFulfilled()) {
					throw new Exception(
							"Requirement implementation error, checkFulfilled() failed after calling "
									+ this.getClass().getName() + ".handle();");
				}
			}
		} catch (Exception ex) {
			log.error("Unable to fulfill requirement '"
					+ this.getClass().getName() + "'", ex);
			throw new RequirementNotFulfilledException(
					"Unable to fulfill requirement "
							+ this.getClass().getCanonicalName(), ex);
		}
		log.info("Requirement '" + this.getClass().getName() + "' fulfilled");

	}

	/**
	 * must return true if the Requirement is already fulfilled
	 * 
	 * @return
	 */
	public abstract boolean checkFulfilled();

	/**
	 * handles (should do everything to fulfill requirement),
	 * {@link RequirementBase#checkFulfilled()} should return true after calling
	 * this method
	 */
	public abstract void handle();

	/**
	 * gets requirement priority, higher values are fulfilled later
	 * 
	 * @return
	 */
	public int getPriority() {
		return priority;
	}

	/**
	 * sets requirement priority, higher values are fulfilled later
	 * 
	 * @param priority
	 */
	public void setPriority(int priority) {
		this.priority = priority;
	}

	// need to have requirement's creation methods at one place to have overview
	// by priority

	public static RequirementBase createAddServer() {
		RequirementBase req = new AddServer();
		return req;
	}

	public static RequirementBase createStartServer() {
		RequirementBase req = new StartServer();
		return req;
	}

	public static RequirementBase createAddSeam() {
		RequirementBase req = new AddSeam();
		return req;
	}
	public static RequirementBase createAddESB() {
		RequirementBase req = new AddESB();
		return req;
	}
	
	public static RequirementBase createAddJBPM() {
		RequirementBase req = new AddJBPM(); 
		return req;
	}

	public static AddJava createAddJava() {
		AddJava req = new AddJava();
		return req;
	}

	public static RequirementBase createSwitchPerspective(String name) {
		RequirementBase req = new SwitchPerspective(name);
		req.setPriority(1);
		return req;
	}

	public static RequirementBase createSetProperties() {
		RequirementBase req = new SetProperties();
		req.setPriority(-99);
		return req;
	}	
	public static RequirementBase createClearWorkspace() {
		RequirementBase req = new ClearWorkspace();
		req.setPriority(-2);
		return req;
	}

	public static RequirementBase createClearProjects() {
		RequirementBase req = new ClearProjects();
		req.setPriority(-1);
		return req;
	}
	public static RequirementBase createPrepareViews() {
		RequirementBase req = new PrepareViews();
		req.setPriority(2);
		return req;
	}

	public static RequirementBase createStopServer() {
		RequirementBase req = new StopServer();
		return req;
	}

	public static RequirementBase createRemoveServer() {
		RequirementBase req = new RemoveServer();
		return req;
	}
	public static RequirementBase createAddRemoteSystem() {
		RequirementBase req = new AddRemoteSystem();
		return req;
	}

	public static RequirementBase createRemoveRemoteSystem() {
		RequirementBase req = new RemoveRemoteSystem();
		return req;
	}
	public static RequirementBase prepareDB () {
		RequirementBase req = new PrepareDB();
		return req;
	}
	
	public static RequirementBase createStopDBServer() {
		RequirementBase req = new StopDBServer();
		return req;
	}
}
