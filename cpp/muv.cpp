
#include <stdio.h>
#include <string>

#define const

template <typename Message> struct View {
	virtual const Message * run() const = 0;
	virtual ~View<Message>() {};
};

struct Processor {
	virtual const Processor *next() const = 0;
};

struct Model {
	class Message;
	virtual ~Model() {};
	virtual const Processor *processor() const = 0;
};

template <typename CurModel> struct ProcessorImpl : public Processor {
	const CurModel * model;
	ProcessorImpl<CurModel>(const CurModel* m) : model(m) { };
	const Processor *next() const {
		const View<typename CurModel::Message> * view = model->view();
		const typename CurModel::Message * msg = view->run();
		delete view;
		const Model * newModel = msg->process(model);
		delete msg;
		return newModel->processor();
	}
};

struct Logined : public Model {
	struct Message {
                const virtual Model * process(const Logined * m) = 0;
		virtual ~Message() {};
        };
	struct Logout : public Message {
		const Model * process(const Logined * m);
	};
	struct Greet : public Message {
		const Model * process(const Logined * m);
	};

	const std::string name;
	Logined(std::string lname) : name(lname) { };

	struct LoginedView : public View<Message> {
		const std::string name;
		LoginedView(std::string lname) : name(lname) {};
		virtual const Message * run() const {
			char buf[16];
			printf("Hello %s", name.c_str());
			fgets(buf, 15, stdin);
			return (*buf == 0 || *buf == '\n' || *buf == '\r')
			     ? static_cast<const Message*>(new Logout())
			     : static_cast<const Message *>(new Greet);
		};
	};

	const View<Message> * view() const {
		return new LoginedView(name);
	};

	const Processor *processor() const {
		return new ProcessorImpl<Logined>(this);
	};
};

struct Logouted : public Model {
	struct Message {
                const virtual Model * process(const Logouted * m) const = 0;
		virtual ~Message() {};
        };
	struct Login : public Message {
		const std::string name;
		Login(std::string lname) : name(lname) { };
		const Model * process(const Logouted * m) const;
	};

	struct LogoutedView : public View<Message> {
		virtual const Message * run() const {
			char buf[16];
			printf("Login: ");
			fgets(buf, 15, stdin);
			return new Login(buf);
		};
	};

	const View<Message> * view() const {
		return new LogoutedView();
	};

	const Processor *processor() const {
		return new ProcessorImpl<Logouted>(this);
	};
};

const Model * Logouted::Login::process(const Logouted * m) const {
	delete m;
	return new Logined(name);
};

const Model * Logined::Logout::process(const Logined * m) const {
	delete m;
        return new Logouted();
};

const Model * Logined::Greet::process(const Logined * m) const {
	return m;
};

int main(int argc, char ** argv) {
	Processor * p = new ProcessorImpl<Logouted>(new Logouted());
	while(true) {
		Processor * pnew = p->next();
		delete p;
		p = pnew;
	}
	return 0;
}
